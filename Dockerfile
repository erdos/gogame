# Stage 1: Compile & build standalone uberjar
FROM docker.io/library/clojure:latest AS jar-builder

WORKDIR /build

# Pre-fetch dependencies for better layer caching
COPY deps.edn ./
RUN clojure -P && clojure -P -T:build

# Copy source code and build configuration
COPY src/ ./src/
COPY resources/ ./resources/
COPY build.clj ./

# Compile sources with direct linking and package uberjar
RUN clojure -T:build uber

# Stage 2: Ahead-of-time compile native binary with GraalVM musl
FROM ghcr.io/graalvm/native-image-community:21-muslib AS native-builder

WORKDIR /build

COPY --from=jar-builder /build/target/gogame-standalone.jar ./target/gogame-standalone.jar

RUN mkdir -p /media /app /tmp \
    && native-image \
        -jar ./target/gogame-standalone.jar \
        --static \
        --libc=musl \
        --no-fallback \
        --features=clj_easy.graal_build_time.InitClojureClasses \
        -H:IncludeResources="public/.*" \
        -H:+ReportExceptionStackTraces

# Stage 3: Minimal scratch runtime
FROM scratch

COPY --from=native-builder /media /media
COPY --from=native-builder /tmp /tmp
COPY --from=native-builder /build/gogame-standalone /app/gogame

EXPOSE 3000
ENV HTTP_PORT=3000
ENV DB_FILE=/media/storage

ENTRYPOINT ["/app/gogame"]
