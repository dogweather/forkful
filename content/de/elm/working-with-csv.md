---
title:                "Arbeiten mit CSV"
html_title:           "Elm: Arbeiten mit CSV"
simple_title:         "Arbeiten mit CSV"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/working-with-csv.md"
---

{{< edit_this_page >}}

## Was & Warum?

Arbeiten mit CSV, auch bekannt als "Comma Separated Values", bezieht sich auf das Lesen und Schreiben von tabellarischen Daten in einem einfachen Textformat. Programmierer nutzen oft CSV, um Daten aus externen Quellen zu importieren oder um Daten in einem Format zu speichern, das leicht von anderen Programmen gelesen werden kann. 

## Wie geht's?

Hier sind einige Beispiele, um CSV-Dateien in Elm zu lesen und zu schreiben:

```Elm
-- Lesen einer CSV-Datei
import Csv.Decode as Decode

-- Daten dekodieren und ausgeben
Decode.decodeString Decode.int """
Name, Alter
John, 25
Jane, 30
""" -- Ergebnis: Ok [ {name = "John", age = 25}, {name = "Jane", age = 30} ]

-- Schreiben von CSV-Daten
import Csv.Encode as Encode

-- Daten kodieren und in eine Zeichenfolge umwandeln
Encode.encode (Encode.list [ {name = "John", age = 25}, {name = "Jane", age = 30} ])
  |> Encode.string -- Ergebnis: "Name, Alter\nJohn, 25\nJane, 30\n"
```

## Tiefentauchen

CSV wurde in den 1970er Jahren als einfaches Datenformat entwickelt, um den Austausch von tabellarischen Daten zwischen verschiedenen Programmen zu ermöglichen. Heutzutage gibt es viele Alternativen wie JSON oder XML, aber CSV bleibt aufgrund seiner Einfachheit und Lesbarkeit beliebt. Die Implementierung von CSV in Elm basiert auf dem Paket "Csv.Encode" und "Csv.Decode", die es ermöglichen, Daten in das entsprechende Format zu konvertieren und zu verarbeiten.

## Siehe auch

- Die offizielle [Elm-Dokumentation](https://guide.elm-lang.org/effects/json.html), die weitere Informationen und Beispiele zu CSV in Elm bietet.