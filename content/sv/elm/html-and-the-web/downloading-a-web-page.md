---
title:                "Hämta en webbsida"
date:                  2024-01-20T17:43:53.139302-07:00
model:                 gpt-4-1106-preview
simple_title:         "Hämta en webbsida"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att ladda ner en webbsida betyder att hämta dess innehåll via internet. Programmerare gör detta för att bearbeta information, fylla appar med data eller spara sidor för offline-användning.

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
