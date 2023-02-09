(ns bot
  (:require
   [chime.core            :as chime]
   [chime.core-async      :refer [chime-ch]]
   [clojure.core.async    :as a]
   [clojure.pprint :as pprint]
   [clojure.string        :as s]
   [clojure.tools.logging :as log]
   [cprop.core            :refer [load-config]]
   [discljord.connections :as c]
   [discljord.events :as e]
   [discljord.messaging   :as m]
   [discljord.permissions :as p])
  (:gen-class))

(defonce state (atom nil))

(defn- now []
  (java.time.ZonedDateTime/now (java.time.ZoneId/of "Europe/London")))

(defn- date->zdt [event-date]
  (try
    (java.time.ZonedDateTime/of
     (java.time.LocalDateTime/parse event-date)
     (java.time.ZoneId/of "Europe/London"))
    (catch Throwable _ nil)))

(defn- time-left [event-date]
  (let [event-start (date->zdt event-date)
        diff          (- (.toEpochMilli (.toInstant event-start)) (.toEpochMilli (.toInstant (now))))
        days          (quot diff 86400000)
        hours         (mod (quot diff 3600000) 24)
        mins          (mod (-> (quot diff 1000)
                               (quot 60))
                           60)]
    {:days days
     :hours hours
     :mins mins}))

(defn- channel-name [event]
  (let [{:keys [short-name event-date]} event
        {:keys [days hours mins]} (time-left event-date)]
    (if (.isAfter (date->zdt event-date) (now))
      (str short-name " " days "D " hours "H " mins "M")
      (str short-name " happening!"))))

(defn- long-message [event]
  (let [{:keys [event-name event-date extra-message]} event
        {:keys [days hours mins]} (time-left event-date)]
    (if (.isAfter (date->zdt event-date) (now))
      (format "There are %d days, %d hours and %d minutes until %s!\n%s" days hours mins event-name (or extra-message ""))
      (str event-name " happening!"))))

(defn- channel-starts-with? [prefix]
  (let [channels (a/<!! (m/get-guild-channels! (:messaging @state) (:guild @state)))]
    (first (filter #(s/starts-with? (:name %) prefix) channels))))

(defn- update-channels []
  (log/info (format "Updating %d events" (count (:events @state))))
  (a/go-loop [events (:events @state)]
    (when-let [event (first events)]
      (log/info "Attempting to update" (:event-name event))
      (if-let [channel (get-in @state [:channels (:event-name event)])]
        (a/<! (m/modify-channel! (:messaging @state) (:id channel) {:name (channel-name event)}))
        (swap!
         state
         assoc-in
         [:channels (:event-name event)]
         (a/<! (m/create-guild-channel!
                (:messaging @state)
                (:guild @state)
                (channel-name event)
                {:type 2
                 :permission-overwrites [{:id (get-in @state [:roles :public])
                                          :type 0
                                          :allow 0
                                          :deny (:connect p/permissions-bit)}]}))))
      (Thread/sleep 1000) ;; Prevent burst rate limits
      (recur (next events)))))

(defn handle-countdown-command [tokens]
  (if (empty? tokens)
    (format
     "The following events are available: \n%s\nUse the shortcode to query `!countdown %s`"
     (s/join "\n" (map #(format "- %s (%s)" (:event-name %) (:short-name %)) (:events @state)))
     (first (map :short-name (:events @state))))
    (if-let [event (first (filter #(= (:short-name %) (first tokens)) (:events @state)))]
      (long-message event)
      (format "Event \"%s\" not found, try one of %s" (first tokens) (s/join ", " (map :short-name (:events @state)))))))

(defn handle-countdown-add-command [tokens]
  (if (.contains [3 4] (count tokens))
    (let [[event-name short-name event-date extra-message] tokens]
      (if-not (date->zdt event-date)
        (format "Invalid format for date. Example format looks like `2023-04-01T00:00:00`")
        (do
          (swap! state assoc :events (conj (:events @state) {:event-name event-name
                                                             :short-name short-name
                                                             :event-date event-date
                                                             :extra-message extra-message}))
          (update-channels)
          (handle-countdown-command []))))
    (format "Sorry, I couldn't understand that event. Expected format is `!countdown-add <event-name:string> <short-name:string> <event-date:date> <extra-message:optional-string>`")))

(defn handle-countdown-rm-command [tokens]
  (if (empty? tokens)
    (format
     "The following events are available: \n%s\nUse the shortcode to remove `!countdown-rm %s`"
     (s/join "\n" (map #(format "- %s (%s)" (:event-name %) (:short-name %)) (:events @state)))
     (first (map :short-name (:events @state))))
    (if-let [event (first (filter #(= (:short-name %) (first tokens)) (:events @state)))]
      (when-let [channel (get-in @state [:channels (:event-name event)])]
        (log/info (a/<!! (m/delete-channel! (:messaging @state) (:id channel))))
        (swap! state assoc :events (remove #{event} (:events @state)))
        (swap! state update-in [:channels] dissoc (:event-name event))
        (update-channels)
        (handle-countdown-command []))
      (format "Event \"%s\" not found, try one of %s" (first tokens) (s/join ", " (map :short-name (:events @state)))))))

;; TODO Make the commands a macro which includes usage metadata
(defn handle-countdown-help-command [tokens]
  "The following commands are available:

- `!countdown` :: View a list of all current countdowns
- `!countdown-add <event-name:string> <short-name:string> <event-date:date> <extra-message:optional-string>` :: Add a new event to track. Event date should look like `2023-04-01T00:00:00`
- `!countdown-rm <short-name:string>` :: Remove an event by the given short name. Also deletes the channel
- `!countdown-help` :: View this help")

(defn handle-countdown-debug-command [_]
  (let [out (java.io.StringWriter.)]
    (pprint/pprint @state out)
    (format "```%s```"  (.toString out))))

(def ^:private command-handlers
  {"!countdown" #'handle-countdown-command
   "!countdown-add" #'handle-countdown-add-command
   "!countdown-rm" #'handle-countdown-rm-command
   "!countdown-help" #'handle-countdown-help-command
   "!countdown-debug" #'handle-countdown-debug-command})

(defn handle-message
  [_event-type {:keys [author channel-id content]}]
  (when (not (:bot author))
    (let [tokens (->> (s/split content #"\s(?=(([^\"]*\"){2})*[^\"]*$)\s*")
                      (map #(s/replace % "\"" "")))]
      (when-let [handler (command-handlers (nth tokens 0))]
        (m/create-message!
         (:messaging @state)
         channel-id
         :content
         (format
          "Hello, <@%s>!\n%s"
          (:id author)
          (handler (rest tokens))))))))

(defn handle-ready
  [_event-type {:keys [user]}]
  (when-not (:id @state)
    (log/info "Connection ready")
    (swap! state assoc
           :id (:id user)
           :channels (->> (map #(channel-starts-with? (:short-name %)) (:events @state))
                          (zipmap (map :event-name (:events @state))))
           :roles {:public (->> (a/<!! (m/get-guild-roles! (:messaging @state) (:guild @state)))
                                (filter #(= (:name %) "@everyone"))
                                first
                                :id)})
    (log/info "Updating current channels")
    (update-channels)))

(def ^:private handlers
  {:message-create [#'handle-message]
   :ready          [#'handle-ready]})

(defn -main [& args]
  (let [{:keys [token guild-id events]} (load-config)
        chimes (chime-ch (chime/periodic-seq (java.time.Instant/now) (java.time.Duration/ofMinutes 5))
                         {:ch (a/chan (a/sliding-buffer 1))})]

    (log/info "Starting bot...")
    (a/go-loop []
      (when (and (a/<! chimes)
                 (not (nil? @state)))
        (update-channels))
      (recur))

    (a/go
      (when (nil? @state)
        (let [event-channel (a/chan 100)
              bot-connection (c/connect-bot! token event-channel :intents #{:guilds :guild-messages})
              messaging-connection (m/start-connection! token)]
          (reset! state {:connection bot-connection
                         :event event-channel
                         :messaging messaging-connection
                         :guild guild-id
                         :events events})
          (try
            (log/info "Setting up message handlers")
            (e/message-pump! event-channel (partial e/dispatch-handlers #'handlers))

            (finally
              (log/info "Exiting")
              (m/stop-connection! messaging-connection)
              (c/disconnect-bot!  bot-connection)
              (map a/close! [chimes event-channel])
              (reset! state nil))))))))
