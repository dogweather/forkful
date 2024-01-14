---
title:                "Haskell: Umwandlung eines Strings in Kleinbuchstaben"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum

Das Konvertieren von Strings in Kleinbuchstaben kann hilfreich sein, um die Vergleichbarkeit von Strings zu verbessern oder um sicherzustellen, dass sie in einem bestimmten Format vorliegen. In Haskell kann dies auf verschiedene Arten erreicht werden.

## Wie

Das Umwandeln von Strings in Kleinbuchstaben ist in Haskell mit der Funktion `toLower` aus dem Modul `Data.Char` möglich. Diese Funktion akzeptiert einen Charakter als Argument und gibt den entsprechenden Kleinbuchstaben zurück.

```Haskell
import Data.Char
toLower 'H'
-- Ausgabe: 'h'
toLower 'a'
-- Ausgabe: 'a'
toLower '7'
-- Ausgabe: '7'
```

Eine andere Möglichkeit ist die Verwendung der Funktion `map`, um `toLower` auf jeden Charakter im String anzuwenden.

```Haskell
import Data.Char
map toLower "HALLO"
-- Ausgabe: "hallo"
```

## Deep Dive

Die Funktion `toLower` verwendet die unterliegende [Unicode-Tabelle](https://unicode-table.com/en/) und berücksichtigt somit auch Sonderzeichen und Akzente. Dies ist besonders wichtig, wenn Strings aus verschiedenen Sprachen verarbeitet werden. Es ist auch möglich, eigene Funktionen zur Konvertierung von Groß- zu Kleinbuchstaben zu definieren, z.B. um bestimmte diakritische Zeichen zu berücksichtigen.

## Siehe auch

- [Haskell Dokumentation zu toLower](https://www.haskell.org/onlinereport/standard-prelude.html#html-decimal)
- [Blogpost zu Unicode und Haskell](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/type-safe-string-manipulation)