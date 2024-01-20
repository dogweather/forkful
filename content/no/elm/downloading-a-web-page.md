---
title:                "Laste ned en nettside"
html_title:           "Elixir: Laste ned en nettside"
simple_title:         "Laste ned en nettside"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# La oss lære Elm!

## Hva & Hvorfor?

Å laste ned en webside er å hente data fra en bestemt URL, slik at den kan brukes eller vises i applikasjonen din. Programmerere gjør dette for å innhente nyttig informasjon fra eksterne kilder.

## Hvordan:

La oss dykke ned i hvordan du gjør dette i Elm. Her er det grunnleggende kodeskjelettet:

```Elm
import Http
import Json.Decode as Decode

-- Definisjon av et HTTP-forespørsel som vil laste ned en webside
loadWebPage : String -> Http.Request String
loadWebPage url =
    Http.request
        { method = "GET"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectStringResponse (\_ result -> result)
        , timeout = Nothing
        , tracker = Nothing
        }

-- Bruker forespørselen for å laste opp websiden
main =
    Http.send handleDownloadResult (loadWebPage "https://www.example.com")
```

Output vil være innholdet på websiden du ba om, eller en feilmelding.

## Dyp Dykk

Elm sitt HTTP-bibliotek ble introdusert i versjon 0.18 og har blitt stabilt og pålitelig verktøy for å laste ned websider. Alternativene innebærer bruk av JavaScript via Elm's porter, noe som ikke alltid er ideelt.

Når det gjelder nedlasting av websider i Elm, det er effektivt fordi Elm håndterer asynkronitet for deg. Du trenger ikke å bekymre deg for callbacks, promises eller async / await som i JavaScript. Elm bruker en arkitektur basert på meldinger, der du sender en forespørsel og får et svar i den togformede Elm-arkitekturen.

## Se Også

For mer informasjon, sjekk ut disse ressursene: 
- Elm's offisielle dokumenter: [Elm HTTP](https://package.elm-lang.org/packages/elm/http/latest/)
- Elm's guide til å håndtere effekter: [Effects](https://guide.elm-lang.org/effects/)