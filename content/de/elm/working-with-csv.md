---
title:                "Elm: Arbeiten mit CSV"
simple_title:         "Arbeiten mit CSV"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/working-with-csv.md"
---

{{< edit_this_page >}}

## Warum

CSV-Dateien sind ein wesentlicher Bestandteil der Datenverarbeitung und -analyse. Sie ermöglichen es uns, große Datenmengen in einem strukturierten Format zu speichern und zu verarbeiten. Mit Elm können wir effektiv mit CSV-Dateien arbeiten, indem wir sie in unsere Anwendungen einbinden und manipulieren. In diesem Artikel erfahren Sie, wie Sie CSV-Dateien in Elm verwenden können.

## Wie man CSV in Elm verwendet

Der erste Schritt ist, die nötigen Pakete zu installieren. Öffnen Sie dazu Ihr Terminal und geben Sie den folgenden Befehl ein:

```
Elm install elm-csv
```

Als nächstes müssen wir die Datei mit den Daten einbinden. Dazu können wir die `load`-Funktion verwenden, die in `elm-csv` enthalten ist. Wir geben die Datei als `Text` ein und verwenden die Funktion `split` um die Daten in ein Array aufzuteilen. Unser Code sieht wie folgt aus:

```
module Main exposing (main)

import Csv
import Html exposing (text)

type alias Model =
    { data : List Csv.Row
    }

init : Model
init =
    { data = [] }

type Msg
    = LoadingCsv
    | CsvLoaded (Result Csv.Error (List Csv.Row))

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadingCsv ->
            ( model, Csv.load "data.csv" |> Task.perform CsvLoaded )

        CsvLoaded result ->
            case result of
                Ok rows ->
                    ( { model | data = rows }, Cmd.none )

                Err error ->
                    ( model, Cmd.none )

view : Model -> Html.Html Msg
view model =
    div [] [ text (List.toString model.data) ]

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
```

Sobald wir diesen Code ausführen, haben wir Zugriff auf die Daten aus unserer CSV-Datei und können sie in unserer Anwendung verwenden.

## Tiefer Eintauchen

Wenn Sie tiefer in die Arbeit mit CSV-Dateien in Elm eintauchen möchten, gibt es einige wichtige Dinge zu beachten:

- Verwenden Sie die `decode`-Funktion, um die Daten in einer CSV-Datei in ein spezifisches Datenmodell zu konvertieren.
- Nutzen Sie die `generate`-Funktion, um neue CSV-Dateien zu erstellen oder bestehende zu aktualisieren.
- Verwenden Sie die `Csv.Encode`-Module, um bestimmte Datentypen in CSV-kompatible Strings zu konvertieren.

Sie können auch die offizielle Elm-Dokumentation zu CSV für weitere Informationen und Beispiele besuchen.

## Siehe auch

- [Offizielle Elm-Dokumentation zu CSV](https://guide.elm-lang.org/effects/http.html)
- [elm-csv Paket](https://package.elm-lang.org/packages/elm-tooling/csv/latest/)
- [Eine einfache Anleitung zur Arbeit mit CSV-Dateien in Elm](https://dev.to/feldmanbrett/how-to-work-with-csv-files-in-elm-1mk1)

Vielen Dank fürs Lesen und viel Spaß beim Arbeiten mit CSV-Dateien in Elm!