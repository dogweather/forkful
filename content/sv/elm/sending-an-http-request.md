---
title:                "Skicka en http-förfrågan"
date:                  2024-01-20T17:59:30.031735-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skicka en http-förfrågan"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Vad & Varför?
HTTP-begäran skickas för att kommunicera med en webbserver, oftast för att hämta eller skicka data. Programmerare använder detta för att låta sina webbapplikationer interagera med andra tjänster eller för att hämta information som ska visas för användaren.

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
