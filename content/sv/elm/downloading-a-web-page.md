---
title:                "Hämta en webbsida"
html_title:           "Elm: Hämta en webbsida"
simple_title:         "Hämta en webbsida"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att ladda ner en webbsida betyder att hämta den från internet och spara den på din dator. Programmerare gör det för att kunna manipulera sidans innehåll eller använda det till andra ändamål.

## Hur man gör:

```elm
module Main exposing (..)

import Browser
import Html exposing (..)
import Http
import Json.Decode

type Msg = ReceivedResponse (Result Http.Error String)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceivedResponse result ->
            case result of
                Ok response ->
                    let
                        pageContent : String
                        pageContent = response
                    in
                        ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

getWebPage : Cmd Msg
getWebPage =
    Http.get
        { url = "https://www.example.com"
        , expect = Http.expectString ReceivedResponse
        }

init : () -> ( Model, Cmd Msg )
init _ =
    ( (), getWebPage )

view : () -> Html Msg
view _ =
    text "Downloading a web page"

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
```

Output:
```
Downloading a web page
```

## Djupdykning:

Att ladda ner en webbsida har funnits sedan internet började och är en central del av webbutveckling. Det finns olika sätt att implementera det, men i Elm används funktionen "Http.get" för att hämta en webbsida och "Http.expectString" för att förvänta sig en sträng som svar. Alternativ till Elm inkluderar andra programmeringsspråk som till exempel Java eller JavaScript.

## Se även:

[https://guide.elm-lang.org/effects/http.html](https://guide.elm-lang.org/effects/http.html)

[https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest/Using_XMLHttpRequest](https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest/Using_XMLHttpRequest)