# Stage 1: Compile & build standalone uberjar
FROM docker.io/library/clojure:latest AS builder

WORKDIR /build

# Pre-fetch dependencies for better layer caching
COPY deps.edn ./
RUN clojure -P && clojure -P -T:build

# Copy source code and build configuration
COPY src/ ./src/
COPY resources/ ./resources/
COPY build.clj ./

# Compile sources and package uberjar
RUN clojure -T:build uber

# Stage 2: Lightweight runtime environment
FROM docker.io/library/eclipse-temurin:21-jre-alpine AS runner

WORKDIR /app

# Copy compiled standalone uberjar from builder stage
COPY --from=builder /build/target/gogame-standalone.jar /app/gogame.jar

EXPOSE 3000
ENV HTTP_PORT=3000
ENV DB_FILE=/media/storage

ENTRYPOINT ["java", "-jar", "/app/gogame.jar"]
