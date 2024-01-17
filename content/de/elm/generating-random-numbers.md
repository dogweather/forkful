---
title:                "Erzeugung zufälliger Zahlen"
html_title:           "Elm: Erzeugung zufälliger Zahlen"
simple_title:         "Erzeugung zufälliger Zahlen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?
Zufallszahlen zu generieren bedeutet, dass der Computer eine Zahl aus einer zufälligen Auswahl von Zahlen auswählt. Programmierer nutzen diese Funktion, um Spiele, Simulationen oder kryptografische Anwendungen zu entwickeln.

## So geht's:
Um Zufallszahlen in Elm zu generieren, kannst du die `random` Bibliothek verwenden. Zuerst musst du sie importieren und dann kannst du die Funktion `generate` verwenden, um eine Zufallszahl zu generieren. Hier ist ein Beispiel, das eine Zufallszahl zwischen 1 und 10 generiert:

```Elm
import Random

Random.generate (\_ -> 1 + Random.int 1 10)
```

Der Rückgabewert ist ein Ergebnis von einem Typ, den die Bibliothek definiert. Du kannst das Ergebnis in einer Variable speichern oder direkt im Code verwenden.

## Tiefgründig:
Die Generierung von Zufallszahlen ist eine wichtige Funktion in der Informatik und hat eine lange Geschichte. Bevor es Computer gab, wurden Zufallszahlen mit Würfeln oder anderen physischen Mitteln erzeugt. Heutzutage gibt es viele Alternativen zur `random` Bibliothek, wie zum Beispiel externe APIs oder zufällige Ereignisse in der Hardware.

Die `random` Bibliothek von Elm verwendet intern seed-based algorithm. Das bedeutet, dass der Computer einen initialen Wert benötigt, um zufällige Zahlen zu generieren. Diese seed-Wert kann manuell gesetzt werden oder mithilfe von `Random.initialSeed` automatisch generiert werden.

## Siehe auch:
- Die offizielle Dokumentation von `random`: https://package.elm-lang.org/packages/elm/random/latest/
- Eine Einführung in die Zufallszahlengenerierung in Elm: https://dev.to/denizdogan/learning-elm-generating-random-numbers-1g72
- Ein Vergleich von verschiedenen Methoden zur Generierung von Zufallszahlen: https://medium.com/@agm1984/generating-random-numbers-in-elm-ce0d733c54