---
title:                "Verwendung eines Zeichensatzes in Großbuchstaben"
html_title:           "Haskell: Verwendung eines Zeichensatzes in Großbuchstaben"
simple_title:         "Verwendung eines Zeichensatzes in Großbuchstaben"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Kapitalisieren einer Zeichenkette bedeutet, den ersten Buchstaben eines Wortes großzuschreiben. Programmierer tun dies, um die Lesbarkeit ihres Codes zu verbessern und bestimmte Konventionen oder Stilrichtlinien einzuhalten.

## Wie geht's?
```Haskell
capitalizedString = map toUpper "hallo welt"
```
Ausgabe: "Hallo Welt"

```Haskell
capitalizedWords = unwords $ map capitalizeFirstWord $ words "dies ist ein beispiel"
    where capitalizeFirstWord (x:xs) = toUpper x:xs
```
Ausgabe: "Dies Ist Ein Beispiel"

## Tiefer tauchen
Das Kapitalisieren von Strings hat seine Wurzeln in der typografischen Konvention, den ersten Buchstaben eines Satzes großzuschreiben. Alternativ kann auch die Funktion "capitalize" aus dem Modul "Data.Char" verwendet werden, um nur den ersten Buchstaben großzuschreiben.

## Siehe auch
[Offizielle Haskell-Dokumentation zum Modul "Data.Char"](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Char.html)