FROM haskell:9.6.7-slim-bullseye as builder

WORKDIR /app
ADD . /app
RUN cabal update && \
    cabal install exe:monalog --install-method=copy \
                              --installdir=/app/artifacts

FROM debian:bullseye-slim
ENV LANG C.UTF-8
WORKDIR /app
COPY --from=builder /app/artifacts/monalog /usr/local/bin/monalog
COPY --from=builder /app/html /app/html
ENTRYPOINT ["/usr/local/bin/monalog"]
