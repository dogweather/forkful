---
title:                "Strings verketten"
html_title:           "Bash: Strings verketten"
simple_title:         "Strings verketten"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Was und Warum?

Das Verketten von Strings ist das Aneinanderreihen von zwei oder mehr Zeichenketten, um eine neue zu bilden. Programmierer machen dies, um Worte, Sätze oder Datenstrukturen in ausgebbarer Textform zu erstellen.

## So geht's:

Lassen Sie uns in der Praxis sehen, wie man Zeichenketten in Haskell verkettet. Betrachten Sie folgenden Code:

```Haskell
main = do   
    let str1 = "Hallo!"   
    let str2 = " Wie geht's?"   
    putStrLn (str1 ++ str2)   
```

Dieser Code gibt folgenden String aus:

```Haskell
"Hallo! Wie geht's?"
```

In Haskell wird '++' benutzt, um zwei Strings zu verketten.

## Tiefereinblick:

Die '++' Funktion für die Verkettung von Zeichenketten wurde bereits in Haskell 1.0 eingeführt und hat sich seitdem nicht verändert.

Alternativ könnten Sie `concat` verwenden, um eine Liste von Strings zu einem einzigen String zu verbinden.

Haskell implementiert das Verketten von Strings durch das Zusammenfügen der internen Listenrepräsentationen der Strings. Deshalb ist das Verketten sehr effizient.

## Siehe auch:

- [Haskell/Lists and tuples](http://en.wikibooks.org/wiki/Haskell/Lists_and_tuples)
- [Haskell Wiki](https://wiki.haskell.org/How_to_work_with_strings)
- [Haskell: Manipulieren von Zeichenketten](https://www.tutorialspoint.com/haskell/haskell_strings.htm)