FROM haskell:9.6.3 as build

WORKDIR /app

COPY zkfold-prover-api.cabal cabal.project /app/

RUN cabal update

COPY . /app

RUN cabal build --enable-executable-static --libexecdir=/usr/local/bin

FROM debian:bullseye-slim

RUN apt-get update && apt-get install -y libgmp10 && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY --from=build /usr/local/bin/zkfold-prover-api /app/zkfold-prover-api

EXPOSE 8080
