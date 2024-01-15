---
title:                "Analisi dei tag html"
html_title:           "Elm: Analisi dei tag html"
simple_title:         "Analisi dei tag html"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/parsing-html.md"
---

{{< edit_this_page >}}

## Perché

Sebbene non sembri la parte più eccitante della programmazione, essere in grado di parsare l'HTML è incredibilmente utile per gli sviluppatori. Questa abilità permette di ottenere informazioni strutturate da pagine web e di manipolarle per creare esperienze utente più dinamiche e personalizzate.

## Come fare

L'Elm fornisce una libreria integrata chiamata "elm/html" che contiene funzioni per parsare l'HTML. Vediamo un esempio di come usarla per ottenere il testo all'interno dell'elemento <h1> di una pagina web:

```Elm
import Html exposing (..)
import Html.Parser exposing (..)
import Http
import Json.Decode as Decode

type Msg
    = ReceivedHtml (Result Http.Error String)

type alias Model =
    { title : String
    , content : String
    }

init : ( Model, Cmd Msg )
init =
    ( { title = "", content = "" }, Http.get "http://sito.com" ReceivedHtml )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceivedHtml (Ok html) ->
            let
                parserResult =
                    parse (all (chompUntil "<h1>") (map .content chompUntil "</h1>")) html
            in
                case parserResult of
                    Ok result ->
                        let
                            (updatedModel, _) =
                                model.title result [] { title = "", content = "" }
                        in
                            ( updatedModel, Cmd.none )

                    Err err ->
                        ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text model.title ]
        , p [] [ text model.content ]
        ]

main : Program () Model Msg
main =
    Html.beginnerProgram
        { model = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
```

In questo codice, utilizziamo la funzione `Http.get` per effettuare una chiamata HTTP per ottenere il contenuto della pagina web. Una volta ricevuto il contenuto, utilizziamo la funzione `Html.Parser.parse` per crearne un risultato parsato utilizzando un parser definito da noi. Nel nostro esempio, abbiamo definito un parser che cerca il contenuto all'interno di un <h1> e lo inserisce nel campo `title` del nostro modello.

## Approfondimento

Oltre alla funzione `parse`, la libreria `elm/html` fornisce molte altre funzioni utili per parsare l'HTML. Ad esempio, ci sono funzioni per parsare elementi specifici, attributi, o anche per creare parser personalizzati. Inoltre, è possibile utilizzare la libreria `elm/parser` per creare parser ancora più avanzati.

Un'altro modo per parsare l'HTML è utilizzare il modulo `built-in Html.Events` che fornisce funzioni come `on`, `targetValue` e `targetChecked`. Queste funzioni possono essere utilizzate per scegliere informazioni da input di form o pulsanti e poi manipolarle tramite funzioni di alto livello come `String.join`, `String.split`, `String.fromList`, etc.

## Vedi anche

- Documentazione ufficiale su `elm/html`: https://package.elm-lang.org/packages/elm/html/latest
- Tutorial su come parsare l'HTML con Elm: https://dev.to/kalaluce/how-to-parse-html-with-elm-3fp2
- Tutorial su come utilizzare il modulo `Html.Events`: https://medium.com/@lysergicordan/input-events-in-elm-686c610859ea