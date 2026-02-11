FROM haskell:9.6.7-slim-bullseye as builder

WORKDIR /app
ADD . /app
RUN cabal update && \
    cabal install exe:monalog --install-method=copy \
                              --installdir=/app/artifacts

FROM debian:bullseye-slim
RUN apt-get update && apt-get install -y \
    openssl \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

ENV LANG C.UTF-8

WORKDIR /app
COPY --from=builder /app/artifacts/monalog /usr/local/bin/monalog
COPY --from=builder /app/html /app/html
COPY --from=builder /app/data /app/data
ENTRYPOINT ["/usr/local/bin/monalog"]
