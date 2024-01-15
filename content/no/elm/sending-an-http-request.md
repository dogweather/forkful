---
title:                "Sending en http-forespørsel"
html_title:           "Elm: Sending en http-forespørsel"
simple_title:         "Sending en http-forespørsel"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hvorfor

Å sende HTTP-forespørsler er en viktig del av å bygge dynamiske og interaktive nettsider og applikasjoner. Enten du ønsker å hente data fra en database, kalle på en ekstern API eller sende skjemainformasjon til en server, er å kunne sende HTTP-forespørsler en essensiell ferdighet for alle Elm-programmerere.

## Slik gjør du det

For å sende en HTTP-forespørsel i Elm, må du først importere HttpClient-modulen og deretter bruke funksjonen `send` sammen med en `Request` og en `Decoder`:

```Elm
import HttpClient exposing (send)
import Json.Decode as Json

type Msg = RequestSucceed | RequestFail Http.Error

sendRequest : Cmd Msg
sendRequest =
  let
    url = "https://api.example.com/users"
    request = HttpClient.get url
    decoder = Json.list User.decodeUser
  in
    send RequestSucceed RequestFail request decoder
```

I dette eksemplet bruker vi funksjonen `get` som er en del av HttpClient-modulen. Vi gir den en URL som vi ønsker å sende en GET-forespørsel til, og deretter dekoder vi svaret til en liste av `User`-objekter ved hjelp av `Json.list` og `User.decodeUser` som vi må definere selv.

`send`-funksjonen returnerer en `Cmd Msg`, som er en kommando som sender en `Msg`-type til programmet når HTTP-forespørselen er fullført. Vi kan deretter håndtere disse meldingene i `update`-funksjonen vår for å endre tilstanden til applikasjonen basert på resultatet av forespørselen.

## Dype dykk

Det finnes flere forskjellige verktøy og biblioteker for å håndtere HTTP-forespørsler i Elm, som gir forskjellige fordeler og ulemper avhengig av hva slags program du bygger. Noen populære alternativer inkluderer `elm/http`, `elm-graphql`, og `elm-peer-http`. Utforske disse kan gi deg en dypere forståelse for hvordan HTTP-forespørsler fungerer i Elm og hvordan de kan integreres i ulike programmer.

## Se også

- [Official Elm Documentation on HTTP](https://package.elm-lang.org/packages/elm/http/latest/) (offisiell Elm-dokumentasjon om HTTP)
- [Using HTTP in Elm with elm/http](https://guide.elm-lang.org/effects/http.html) (bruksanvisning for HTTP i Elm med `elm/http`)
- [Making HTTP requests with elm-graphql](https://dev.to/paulvarache/graphql-with-elm-557e) (hvordan sende HTTP-forespørsler med `elm-graphql`)
- [Elm packages tagged with http](https://package.elm-lang.org/packages/tags/http/latest/) (oversikt over ulike Elm-pakker som er relatert til HTTP)