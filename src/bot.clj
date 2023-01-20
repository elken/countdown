(ns bot
  (:require
   [clojure.core.async    :as a]
   [discljord.connections :as c]
   [discljord.messaging   :as m]
   [discljord.permissions :as p]
   [cprop.core            :refer [load-config]]
   [clojure.string        :as s])
  (:gen-class))

(defn- now []
  (java.time.ZonedDateTime/now (java.time.ZoneId/of "Europe/London")))

(defn -main [& args]
  (let [event-ch      (a/chan 100)
        {:keys [token guild-id message-prefix event-date]} (load-config)
        connection    (c/connect-bot! token event-ch :intents #{:guilds :guild-messages})
        conn          (m/start-connection! token)
        event-start   (java.time.ZonedDateTime/of (java.time.LocalDateTime/parse event-date) (java.time.ZoneId/of "Europe/London"))]

    (defn channel-name []
      (let [diff          (- (.toEpochMilli (.toInstant event-start)) (.toEpochMilli (.toInstant (now))))
            days          (quot diff 86400000)
            hours         (mod (quot diff 3600000) 24)
            mins          (mod (-> (quot diff 1000)
                                   (quot 60))
                               60)]
        (if (.isAfter event-start (now))
          (str message-prefix " " days "D " hours "H " mins "M")
          (str message-prefix " happening!"))))

    (defn channel-starts-with? [prefix]
      (let [channels (a/<!! (m/get-guild-channels! conn guild-id))]
        (first (filter #(s/starts-with? (:name %) prefix) channels))))

    (defn public-role []
      (let [roles (a/<!! (m/get-guild-roles! conn guild-id))]
        (first (filter #(= (:name %) "@everyone") roles))))

    (try
      (loop []
        (let [[event-type] (a/<!! event-ch)
              channel (channel-starts-with? message-prefix)]

          (if channel
            (a/<!! (m/modify-channel! conn (:id channel) {:name (channel-name)}))
            (a/<!! (m/create-guild-channel!
                    conn
                    guild-id
                    (channel-name)
                    {:type 2
                     :permission-overwrites [{:id (:id (public-role))
                                              :type 0
                                              :allow 0
                                              :deny (:connect p/permissions-bit)}]})))

          ;; TODO This could probably do with a Thread/sleep
          (when-not (= :disconnect event-type)
            (recur))))
      (finally
        (m/stop-connection! conn)
        (c/disconnect-bot!  connection)
        (a/close!           event-ch)))))
