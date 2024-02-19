---
aliases:
- /no/elm/reading-a-text-file/
date: 2024-01-20 17:54:24.222556-07:00
description: "\xC5 lese en tekstfil betyr \xE5 hente den lagrede tekstdataen fra en\
  \ fil. Programmerere gj\xF8r dette for \xE5 behandle, vise eller manipulere innhold\
  \ som\u2026"
lastmod: 2024-02-18 23:08:53.827712
model: gpt-4-1106-preview
summary: "\xC5 lese en tekstfil betyr \xE5 hente den lagrede tekstdataen fra en fil.\
  \ Programmerere gj\xF8r dette for \xE5 behandle, vise eller manipulere innhold som\u2026"
title: Lese en tekstfil
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å lese en tekstfil betyr å hente den lagrede tekstdataen fra en fil. Programmerere gjør dette for å behandle, vise eller manipulere innhold som konfigurasjoner eller brukerdata.

## Hvordan:
Elm har ikke innebygd støtte for å lese filer direkte fra filsystemet, da det primært kjøres i nettlesere og fokuserer på sikkerhet. Men, du kan bruke en filopplastningsinput for å be brukere velge en tekstfil, og så lese innholdet med fil-API-et i JavaScript. Her er en enkel måte å gjøre det på:

```Elm
module Main exposing (main)

import Browser
import File exposing (File)
import File.Select as Select
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Ports

-- SUBSCRIPTIONS --

subscriptions : Model -> Sub Msg
subscriptions model =
    Select.files (List.head model.files)

-- MODEL --

type alias Model =
    { files : List File
    , content : Maybe String
    }

initialModel : Model
initialModel =
    { files = []
    , content = Nothing
    }

-- UPDATE --

type Msg
    = FilesSelected (List File)
    | FileContentRead String

update : Msg -> Model -> Model
update msg model =
    case msg of
        FilesSelected files ->
            { model | files = files }

        FileContentRead content ->
            { model | content = Just content }

-- VIEW --

view : Model -> Html Msg
view model =
    div []
        [ button [ onClick (Select.filesSelect "text/*") ] [ text "Velg en fil" ]
        , div [] (maybeToList (text << Maybe.withDefault "Ingen fil valgt" model.content))
        ]
        
-- PORTS --

port module Ports exposing (..)

port readFile : File -> Cmd msg

-- INTEROP --

fileReader : Sub Msg
fileReader =
    Ports.readFile <|
        Select.single <| \file ->
            FilesSelected [file]
            
-- MAIN --

main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> (initialModel, Cmd.none)
        , view = view
        , update = update
        , subscriptions = \_ -> fileReader
        }
```

## Dykke dypere:
Historisk sett har nettleser-baserte språk som Elm unngått direkte filsystem-tilgang for sikkerhet. Elm henter filer gjennom interaksjoner som fildialoger eller dra-og-slipp. I mer server-orienterte omgivelser brukes programmeringsspråk med direkte filsystemtilgang, som Node.js eller Python. Elm bruker JavaScript-interoperabilitet (Ports) for å håndtere fillesing.

## Se også:
- Elm File API dokumentasjon: [Elm File](https://package.elm-lang.org/packages/elm/file/latest/)
- Elm Ports for interoperabilitet: [Elm Ports](https://guide.elm-lang.org/interop/ports.html)
