FROM haskell:9.2.8 AS base

WORKDIR /servidor

FROM base AS build
COPY rinha-fx-puro.cabal ./
RUN mkdir app
COPY app/Main.hs ./app/Main.hs
COPY src ./src/
ENV CABAL_DIR=/servidor/bin
RUN cabal user-config update
RUN cabal update && cabal install rinha-fx-puro
COPY ./.env.prd ./
RUN touch ./bin/.env
RUN cat .env.prd >> ./bin/.env

FROM base
COPY --from=build /servidor/bin ./

EXPOSE 80

USER www-data:www-data
ENTRYPOINT ["./bin/rinha-fx-puro"]
