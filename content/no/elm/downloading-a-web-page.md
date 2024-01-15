---
title:                "Hente en nettside"
html_title:           "Elm: Hente en nettside"
simple_title:         "Hente en nettside"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor laste ned en nettside? Det kan være flere grunner til å gjøre dette, for eksempel å få en lokal kopi av en side for offline bruk, analysere kildekoden eller bare for å lagre informasjon.

## Slik gjør du det

Å laste ned en nettside i Elm er enkelt. Først må vi importere `Http`-biblioteket:

```elm
import Http
```

Deretter definerer vi en `Command` som bruker `Http.getString`-funksjonen for å hente data fra en spesifikk URL:

```elm
fetchPage : Cmd Msg
fetchPage =
    Http.getString "https://www.example.com"
        |> Http.send GotPage
```

Merk at `GotPage` her er en `Msg`-type som må defineres i din Elm-applikasjon.

Vi kan også legge til en `Http.expectString`-funksjon for å håndtere eventuelle feil som kan oppstå under nedlastingen:

```elm
import Http

type Msg
    = GotPage (Result Http.Error String)

fetchPage : Cmd Msg
fetchPage =
    Http.getString "https://www.example.com"
        |> Http.send GotPage
        |> Http.expectString GotPageError

GotPageError : Http.Error -> Msg
GotPageError err =
    -- håndter feil her
```

Når dataen er lastet ned, vil den bli sendt til `GotPage`-funksjonen, som vi må håndtere i vår `update`-funksjon.

## Utforsk videre

Det er mange ulike måter å laste ned og behandle nettsider i Elm på. Her er noen nyttige ressurser for å lære mer:

- [Offisiell Elm-dokumentasjon for Http-biblioteket](https://package.elm-lang.org/packages/elm/http/latest/)
- [Tutorial: Making HTTP Requests in Elm](https://thoughtbot.com/blog/making-http-requests-in-elm)
- [Elm tutorial: Fetching data from an API using Elm's Http module](https://medium.com/@jsgrt/elmtutorial-fetching-data-from-an-api-using-elms-http-module-106c777ec15c)

## Se også

- [Elm sin offisielle nettside](https://elm-lang.org/)
- [Elm på GitHub](https://github.com/elm)
- [Elm på Reddit](https://www.reddit.com/r/elm/)