---
title:                "Arbeiten mit CSV-Dateien"
date:                  2024-01-19
html_title:           "Arduino: Arbeiten mit CSV-Dateien"
simple_title:         "Arbeiten mit CSV-Dateien"

category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
Arbeiten mit CSV (Comma-Separated Values) bedeutet, Daten in Textform zu handhaben, die durch Kommas getrennt sind. Programmierer nutzen CSV für einfaches Importieren und Exportieren von Daten in und aus Tabellenformaten.

## How to:
Elm hat keine eingebaute CSV-Behandlung, daher nutzen wir `elm-csv`, ein Third-Party-Package, für das Parsen und Schreiben. Installiere es mit `elm install ericgj/elm-csv`.

```Elm
import Csv

exampleCsv : String
exampleCsv = 
    "Name,Age,City\nJohn Doe,34,New York\nJane Smith,28,Los Angeles"

parseCsv : Csv.Decode.Decoder (List String)
parseCsv =
    Csv.Decode.row Csv.Decode.string

main =
    case Csv.Decode.fromString parseCsv exampleCsv of
        Ok result ->
            -- Behandle die Daten hier
            String.fromString (List.toString result)

        Err message ->
            -- Fehlerbehandlung
            String.fromString message
```
Ausgabe dieses Beispiels wäre eine Liste von Listen: `["Name","Age","City"],["John Doe","34","New York"],["Jane Smith","28","Los Angeles"]`.

## Deep Dive
CSV ist seit den 1970er Jahren im Einsatz und ist ein verbreitetes Format für Dateninteroperabilität. Alternativen wie JSON oder XML bieten mehr Struktur, sind aber schwerwiegender. In Elm kannst du mit `elm/parser` auch eigene Parsers schreiben, was mehr Kontrolle bietet, aber komplexer ist.

## See Also
- Elm-lang offizielle Seite: [https://elm-lang.org/](https://elm-lang.org/)
- `elm/parser` für eigene Parser: [https://package.elm-lang.org/packages/elm/parser/latest/](https://package.elm-lang.org/packages/elm/parser/latest/)
