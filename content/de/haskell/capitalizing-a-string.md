---
title:                "String in Großbuchstaben umwandeln"
date:                  2024-01-19
html_title:           "C: String in Großbuchstaben umwandeln"
simple_title:         "String in Großbuchstaben umwandeln"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Großschreibung eines Strings bedeutet, alle Buchstaben in Großbuchstaben umzuwandeln. Programmierer nutzen sie, um Konsistenz für Datenvergleiche oder Benutzeroberflächen zu gewährleisten.

## How to:
```Haskell
import Data.Char(toUpper)

capitalize :: String -> String
capitalize = map toUpper

-- Beispielnutzung:
main = putStrLn (capitalize "Haskell ist toll!")
```
Output:
```Haskell
HASKELL IST TOLL!
```

## Deep Dive
Die Großschreibung von Zeichenketten ist keine neue Idee. In den ersten Tagen der Computer gab es nur Großbuchstaben! Die Funktion `toUpper` in Haskell setzt diese Tradition fort, adaptiert sie aber für die moderne, vielschichtige Welt der Textverarbeitung. Alternativ könnten Programmierer auch externen Bibliotheken wie `text` für verbesserte Performance bei großen Textmengen nutzen. Die Implementierung der `toUpper`-Funktion berücksichtigt Unicode, sodass sie auch mit nicht-ASCII-Zeichen funktioniert.

## See Also
- Haskell `Data.Char` Modul: https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Char.html
- `text` Paket für effiziente Stringverarbeitung: https://hackage.haskell.org/package/text
