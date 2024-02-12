---
title:                "Scaricare una pagina web"
aliases:
- it/elm/downloading-a-web-page.md
date:                  2024-01-20T17:43:48.272053-07:00
model:                 gpt-4-1106-preview
simple_title:         "Scaricare una pagina web"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? (Cos'è e Perché?)

Scaricare una pagina web significa recuperarne i contenuti via HTTP per usarli in una app. I programmatori lo fanno per accedere a dati dinamici, integrare servizi esterni, e alimentare le applicazioni con contenuti aggiornati.

## How to: (Come fare:)

In Elm, usare `Http` per scaricare una pagina web è diretto. Creiamo una richiesta e gestiamo la risposta.

```Elm
import Http
import Html exposing (Html, text)
import Json.Decode as Decode

type Msg = GotText String | RequestFailed Http.Error

type alias Model = 
    { content : String
    , error : Maybe String
    }

init : ( Model, Cmd Msg )
init =
    ( { content = "", error = Nothing }
    , fetchPage "https://example.com"
    )

fetchPage : String -> Cmd Msg
fetchPage url =
    Http.get
        { url = url
        , expect = Http.expectString GotText
        }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText text ->
            ( { model | content = text, error = Nothing }, Cmd.none )

        RequestFailed err ->
            ( { model | error = Just (toString err) }, Cmd.none )

view : Model -> Html Msg
view model =
    case model.error of
        Just err ->
            text ("Request failed: " ++ err)

        Nothing ->
            text model.content
```
Il codice sopra inizializza una richiesta al caricamento dell'app e visualizza il contenuto scaricato o un errore.

## Deep Dive (Approfondimento)

Elm rende il download di pagine web sicuro e gestibile. La versione attuale, 0.19.1, continua a utilizzare `Http` dalla 0.18, ma con miglioramenti. In alternativa, si potrebbe usare JavaScript, ma perdendo i benefici della tipizzazione forte di Elm. I dettagli sull'implementazione riguardano principalmente la gestione degli errori e degli effetti, affrontando la natura asincrona delle richieste HTTP.

## See Also (Vedi anche)

- Elm `Http` package: [package.elm-lang.org/packages/elm/http/latest](https://package.elm-lang.org/packages/elm/http/latest)
- Elm JSON Decode: [guide.elm-lang.org/interop/json.html](https://guide.elm-lang.org/interop/json.html)
