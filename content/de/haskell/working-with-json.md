---
title:                "Arbeiten mit JSON"
html_title:           "Haskell: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/working-with-json.md"
---

{{< edit_this_page >}}

## Was ist JSON und warum arbeiten Programmierer damit?

JSON (JavaScript Object Notation) ist ein leichtgewichtiges Datenformat, das verwendet wird, um Daten zwischen verschiedenen Anwendungen auszutauschen. Es basiert auf der Syntax von JavaScript, ist jedoch unabhängig von einer spezifischen Sprache. Programmierer nutzen JSON, um strukturierte Daten zu speichern und austauschbar zu machen, da es einfach zu lesen und zu interpretieren ist.

## Wie kann man mit JSON arbeiten?

Das Arbeiten mit JSON in Haskell ist sehr einfach und intuitiv. Zunächst sollte das Modul `Data.Aeson` importiert werden. Dann kann man JSON-Daten in Haskell-Datenstrukturen (z.B. Listen oder Records) umwandeln, indem man die Funktion `decode` verwendet. Umgekehrt kann man Haskell-Datenstrukturen in JSON-Daten umwandeln, indem man die Funktion `encode` verwendet. Hier ein Beispiel:

```Haskell
import Data.Aeson

-- JSON-Daten
json = "[1,2,3]"

-- Umwandlung in eine Haskell-Liste
list = decode json :: Maybe [Int]

-- Ausgabe: Just [1,2,3]
print list

-- Haskell-Liste in JSON-Daten umwandeln
newJson = encode [4,5,6]

-- Ausgabe: "[4,5,6]"
print newJson
```

## Tiefergehende Informationen zu JSON in Haskell

JSON wurde ursprünglich von Douglas Crockford entwickelt und erlangte schnell große Beliebtheit aufgrund seiner Einfachheit und Flexibilität. Obwohl es in erster Linie für die Verwendung in JavaScript gedacht war, wird es heute von vielen Programmiersprachen, einschließlich Haskell, unterstützt.

Es gibt auch alternative Bibliotheken für die Arbeit mit JSON in Haskell, wie z.B. `json`, `json-autotype` oder `json-data`.

Die Bibliothek `Data.Aeson` implementiert JSON mithilfe von algebraischen Datentypen, was es ermöglicht, JSON-Daten direkt in Haskell-Datenstrukturen umzuwandeln und umgekehrt. 

## Weitere Informationen und Quellen

- [Offizielle Dokumentation von Aeson](https://hackage.haskell.org/package/aeson)