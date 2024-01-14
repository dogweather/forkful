---
title:                "Haskell: String großschreiben"
simple_title:         "String großschreiben"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Kapitalisieren von Strings ist eine grundlegende und häufig verwendete Funktion in der Programmierung. Es ermöglicht es uns, Strings in Großbuchstaben umzuwandeln, was in vielen Fällen nützlich sein kann. In diesem Artikel werden wir lernen, wie man Strings in Haskell kapitalisiert und einen Einblick in die Funktionsweise dieser Operation erhalten.

## Wie geht man vor?

Um einen String in Haskell zu kapitalisieren, gibt es verschiedene Möglichkeiten. Eine davon ist die Verwendung der Funktion `toUpper` aus dem Modul `Data.Char`. Diese Funktion wandelt jeden Buchstaben in einem String in den entsprechenden Großbuchstaben um. Hier ist ein Beispiel dafür:

```Haskell
import Data.Char

-- Funktion zum Kapitalisieren des ersten Buchstaben eines Strings
capitalizeFirst :: String -> String
capitalizeFirst (x:xs) = toUpper x : xs

main = do
    let myString = "hallo, ich bin ein String!"
    putStrLn $ capitalizeFirst myString

-- Output: Hallo, ich bin ein String!
```

In diesem Beispiel sehen wir, wie die Funktion `capitalizeFirst` mithilfe von `toUpper` den ersten Buchstaben eines Strings in Großbuchstaben umwandelt. Wir können diese Funktion auch auf den gesamten String anwenden, indem wir `map` verwenden, wie im folgenden Beispiel gezeigt:

```Haskell
import Data.Char

-- Funktion zum Kapitalisieren eines ganzen Strings
capitalizeString :: String -> String
capitalizeString s = map toUpper s

main = do
    let myString = "hallo, ich bin ein String!"
    putStrLn $ capitalizeString myString

-- Output: HALLO, ICH BIN EIN STRING!
```

Beide Funktionen geben das gleiche Ergebnis zurück, aber sie verwenden unterschiedliche Methoden, um das Ziel zu erreichen. Eine weitere Alternative ist die Verwendung der `words` Funktion, die den String in eine Liste von Wörtern aufteilt und dann mit der `map` Funktion jeden ersten Buchstaben in Großbuchstaben umwandelt. Hier ist ein Beispiel dafür:

```Haskell
import Data.Char

-- Funktion zum Kapitalisieren jedes ersten Buchstabens in einem String
capitalizeWords :: String -> String
capitalizeWords s = unwords $ map capitalizeFirst (words s)

main = do
    let myString = "hallo, ich bin ein String!"
    putStrLn $ capitalizeWords myString

-- Output: Hallo, Ich Bin Ein String!
```

In diesem Beispiel sehen wir, wie wir `words` und `map` verwenden, um jeden ersten Buchstaben jedes Worts im String zu kapitalisieren und dann die Wörter mit `unwords` wieder zu einem String zusammenfügen.

## Tiefergehende Informationen

Das Kapitalisieren von Strings ist eine relativ einfache Operation, aber es gibt einige Dinge, die wir beachten sollten. Zum Beispiel sehen wir in den obigen Beispielen, dass die Funktionen `capitalizeFirst` und `capitalizeWords` nicht berücksichtigen, ob der Buchstabe bereits groß geschrieben ist. Daher würde die Eingabe "HALLO, ICH BIN EIN STRING!" das gleiche Ergebnis zurückgeben wie "hallo, ich bin ein string!".

Um diese Einschränkung zu umgehen, können wir die Funktion `toLower` aus dem Modul `Data.Char` verwenden, die jeden Buchstaben in Kleinbuchstaben umwandelt. Dadurch können wir sicherstellen, dass nur der erste Buchstabe in einem String groß geschrieben wird, unabhängig davon, wie der Rest des Strings aussieht.

## Siehe auch

- [Haskell Data.Char Modul](https://hackage.haskell.org/package/base/docs/Data-Char.html)
- [Haskell String Funktionen](https://www.tutorialspoint.com/haskell/haskell_strings.htm)
- [Haskell map Funktion](https://wiki.haskell.org/Map)