---
title:                "Een tekstbestand lezen"
aliases:
- nl/elm/reading-a-text-file.md
date:                  2024-01-28T22:05:07.045759-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een tekstbestand lezen"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/elm/reading-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Een tekstbestand lezen is het ophalen van inhoud uit een bestand dat is gestructureerd als leesbare tekst, in plaats van binaire gegevens. Programmeurs lezen tekstbestanden om gegevens, configuraties te benaderen, of om grote hoeveelheden tekst in hun applicaties te importeren.

## Hoe:

Elm richt zich voornamelijk op front-end webontwikkeling, waarbij directe toegang tot het bestandssysteem niet mogelijk is vanwege veiligheidsredenen. In plaats daarvan verwerk je bestandsuploads door gebruikers. Hier is hoe je een tekstbestand kunt lezen dat een gebruiker selecteert:

```Elm
module Main exposing (..)

import Browser
import File exposing (File)
import File.Selector as Selector
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

type alias Model =
    { fileContent : String }

type Msg
    = SelectFile
    | ReceiveFileContent (Result () String)

init : Model
init =
    { fileContent = "" }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    geval msg van
        SelectFile ->
            (model, fileSelectCmd)

        ReceiveFileContent (Ok content) ->
            ({ model | fileContent = content }, Cmd.none)

        ReceiveFileContent (Err _) ->
            (model, Cmd.none)

fileSelectCmd : Cmd Msg
fileSelectCmd =
    File.select [ Selector.accept "text/*" ] { onDone = ReceiveFileContent }

view : Model -> Html Msg
view model =
    div []
        [ button [ onClick SelectFile ] [ text "Selecteer een tekstbestand" ]
        , div [] [ text model.fileContent ]
        ]

main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }
```

Draai de code in je browser, klik op de knop en selecteer een tekstbestand. Het toont de inhoud in je Elm-app.

## Diepgaande duik

Elm leest geen bestanden direct van het bestandssysteem van de server - het is niet ontworpen voor server-side operaties. In plaats daarvan beheert Elm het invoeren van bestanden via de File API in de browser, doorgaans getriggerd door een gebruikersactie, zoals een bestandsselectie of een slepen-en-neerzetten actie. Het is een veiligheidsmaatregel.

In het verleden heb je misschien JavaScript en Node.js gebruikt om server-side bestanden te lezen, of XMLHttpRequest (XHR) voor client-side lezen zonder gebruikersinteractie. Deze hebben verschillende beveiligingsmodellen en capaciteiten.

De `File` en `File.Selector` modules in Elm maken het redelijk soepel om bestandslezen in de browser te hanteren, maar onthoud de filosofie van "geen neveneffecten" van Elm. Dat betekent dat bestandslezen strikt gecontroleerd wordt, met expliciete gebruikersacties vereist. Ook vereist het parseren en decoderen van bestandsinhoud zorg om te matchen met Elm’s sterke typisering.

## Zie ook

- Officiële Elm File API documentatie: https://package.elm-lang.org/packages/elm/file/latest/
- Een gids voor Elm’s commando's en abonnementen (voor het begrijpen van asynchrone operaties): https://guide.elm-lang.org/effects/
- Elm Discuss voor vragen en gemeenschapsinteractie: https://discourse.elm-lang.org/
