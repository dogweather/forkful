---
date: 2024-01-20 17:59:30.031735-07:00
description: "Hur man g\xF6r: HTTP-beg\xE4ran \xE4r en fundamental del av webbutveckling.\
  \ I Elm hanteras detta elegant med hj\xE4lp av `Http`-modulens funktioner. Det hela\
  \ b\xF6rjade\u2026"
lastmod: '2024-04-05T21:53:39.161156-06:00'
model: gpt-4-1106-preview
summary: "HTTP-beg\xE4ran \xE4r en fundamental del av webbutveckling."
title: "Skicka en http-f\xF6rfr\xE5gan"
weight: 44
---

## Hur man gör:
```Elm
import Http
import Json.Decode as Decode

type Msg
    = GotResponse (Result Http.Error String)

-- Definiera hur svaret ska tolkas
responseDecoder : Decode.Decoder String
responseDecoder =
    Decode.string

-- Skicka en HTTP GET-begäran
sendRequest : Cmd Msg
sendRequest =
    Http.get
        { url = "https://api.exempel.se/data"
        , expect = Http.expectString GotResponse responseDecoder
        }

-- Hanterar svaret i update-funktionen
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotResponse (Ok data) ->
            ({ model | data = data }, Cmd.none)

        GotResponse (Err error) ->
            (model, Cmd.none)

-- Starta begäran från init eller någonstans
init : () -> (Model, Cmd Msg)
init _ =
    (initialModel, sendRequest)
```

## Djupdykning
HTTP-begäran är en fundamental del av webbutveckling. I Elm hanteras detta elegant med hjälp av `Http`-modulens funktioner. Det hela började med Elm 0.18, som introducerade `Http`-paketet och dess tillvägagångssätt för att hantera effekter i en ren och funktionell stil. Alternativ till Elm's inbyggda bibliotek innefattar tredjeparts-paket, men dessa används sällan tack vare Elm's robusta kärnbibliotek. När du använder `Http.get` eller `Http.post` skapas kommandon (Cmd Msg) som hanteras av Elm's runtime för att utföra sidoeffekter. Detta upprätthåller Elm's arkitektur där all kod är ren och sidoeffektfri, medan fortfarande möjliggör interaktioner som HTTP-begäran.

## Se Även
- Elm's officiella HTTP-paketdokumentation: [https://package.elm-lang.org/packages/elm/http/latest/](https://package.elm-lang.org/packages/elm/http/latest/)
- JSON-dekodning i Elm: [https://package.elm-lang.org/packages/elm/json/latest/](https://package.elm-lang.org/packages/elm/json/latest/)
- Elm Guide på att hantera effekter: [https://guide.elm-lang.org/effects/](https://guide.elm-lang.org/effects/)
