---
title:                "Haskell: Unterzeichenfolgen extrahieren"
simple_title:         "Unterzeichenfolgen extrahieren"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum

Substring-Extraktion ist eine nützliche Programmierfunktion, die es uns ermöglicht, Teilzeichenketten aus anderen Zeichenketten zu extrahieren. Dies kann hilfreich sein, wenn wir bestimmte Informationen aus längeren Texten oder Daten extrahieren möchten.

## So geht's

Um Substrings in Haskell zu extrahieren, können wir die `take`- und `drop`-Funktionen verwenden. Die `take`-Funktion nimmt als Argument die Anzahl der gewünschten Zeichen und die ursprüngliche Zeichenkette und gibt die ersten n Zeichen als Substring zurück. Die `drop`-Funktion hingegen gibt die Zeichenkette ohne die ersten n Zeichen als Substring zurück.

Ein Beispielcode sieht so aus:

```Haskell
-- Hier nehmen wir die ersten drei Zeichen des Strings "Haskell" als Substring
take 3 "Haskell"
-- Output: "Has"

-- Hier geben wir alle Zeichen des Strings "Haskell" außer den ersten drei als Substring zurück
drop 3 "Haskell"
-- Output: "kell"
```

## Tieferes Eintauchen

In Haskell kann Substring-Extraktion auch mit der Funktion `substring` aus dem `Data.Text`-Modul durchgeführt werden. Diese Funktion nimmt drei Argumente: die Startposition des Substrings, die Länge des gewünschten Substrings und die ursprüngliche Zeichenkette.

Ein Beispielcode sieht so aus:

```Haskell
import Data.Text (Text, substring)

-- Hier extrahieren wir den Substring aus dem Tripel "Haskell-Programmierung"
substring 0 6 "Haskell-Programmierung"
-- Output: "Haskell"
```

Es ist wichtig zu beachten, dass die Startposition des Substrings bei 0 beginnt.

## Siehe auch

- [Haskell-Dokumentation zur `take`-Funktion](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html#v:take)
- [Haskell-Dokumentation zur `drop`-Funktion](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html#v:drop)
- [Haskell-Dokumentation zur `substring`-Funktion](https://hackage.haskell.org/package/text-1.2.4.1/docs/Data-Text.html#v:substring)