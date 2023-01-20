FROM clojure:temurin-11-alpine AS build-env
MAINTAINER Ellis Kenyő <me@elken.dev>
COPY . /app
WORKDIR /app
RUN clojure -T:build uber

FROM gcr.io/distroless/java11-debian11
COPY --from=build-env /app /app
WORKDIR /app
CMD ["target/countdownBot-uber.jar"]
