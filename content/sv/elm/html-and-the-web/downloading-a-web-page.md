---
date: 2024-01-20 17:43:53.139302-07:00
description: "Hur man g\xF6r: Historiskt sett har webbsidor h\xE4mtats med m\xE5nga\
  \ olika tekniker, fr\xE5n enkla HTTP-anrop till komplexa webbskrapningsverktyg.\
  \ Elm \xE4r en\u2026"
lastmod: '2024-04-05T21:53:39.163224-06:00'
model: gpt-4-1106-preview
summary: "Historiskt sett har webbsidor h\xE4mtats med m\xE5nga olika tekniker, fr\xE5\
  n enkla HTTP-anrop till komplexa webbskrapningsverktyg."
title: "H\xE4mta en webbsida"
weight: 42
---

## Hur man gör:
```Elm
import Http
import Html exposing (Html, text)
import Json.Decode as Decode

type Msg = GotPageContent (Result Http.Error String)

type alias Model = Maybe String

init : Model
init = Nothing

update : Msg -> Model -> (Model, Cmd Msg)
update (GotPageContent result) _ =
    case result of
        Ok pageContent ->
            (Just pageContent, Cmd.none)
        
        Err _ ->
            (Just "Could not load the page.", Cmd.none)

view : Model -> Html Msg
view model =
    case model of
        Just content ->
            text content
        
        Nothing ->
            text "Loading..."

subscription : Model -> Sub Msg
subscription _ =
    Sub.none

main : Program () Model Msg
main =
    Html.program
        { init = (init, fetchPage "http://example.com")
        , view = view
        , update = update
        , subscriptions = subscription
        }

fetchPage : String -> Cmd Msg
fetchPage url =
    Http.get
        { url = url
        , expect = Http.expectString GotPageContent
        }

decoder : Decode.Decoder String
decoder = 
    Decode.string
```

## Fördjupning
Historiskt sett har webbsidor hämtats med många olika tekniker, från enkla HTTP-anrop till komplexa webbskrapningsverktyg. Elm är en funktionell programmeringsspråk designad för webbutveckling, fokuserad på säkerhet och underhållbarhet. I Elm hanterar vi sidhämtning genom `Http`-modulen. Alternativ till Elm för att ladda ner webbsidor inkluderar JavaScript med XMLHttpRequest eller Fetch API, eller serversidespråk som Python med requests-biblioteket.

Elms Http-paket använder "tasks", vilket är Elm-specifika asynkrona operationer. Det är skräddarsytt för att jobba med Elm Runtime för att hantera sidoeffekter som nätverksanrop. Http.get-definierar hur du hämtar innehållet och vad du förväntar dig som svar - i vårt fall, en sträng. Felet hanteras i update-funktionen, där användaren antingen får innehållet eller ett felmeddelande.

## Se även
- Elm's officiella [Http-paket dokumentation](https://package.elm-lang.org/packages/elm/http/latest/)
- En guide till [Elms arkitektur](https://guide.elm-lang.org/architecture/)
- [Json.Decode](https://package.elm-lang.org/packages/elm/json/latest/Json-Decode) för att hantera JSON i Elm
