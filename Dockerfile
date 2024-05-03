FROM haskell:9.8.2 AS base

WORKDIR /servidor

FROM base AS build
COPY rinha-fx-puro.cabal ./
RUN mkdir app
COPY app/Main.hs ./app/Main.hs
COPY src ./src/
RUN cabal update && cabal install rinha-fx-puro

FROM base
COPY --from=build /servidor /servidor

EXPOSE 80

ENTRYPOINT ["rinha-fx-puro"]
