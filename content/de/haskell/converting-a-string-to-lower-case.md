---
title:                "Umwandlung eines Strings in Kleinbuchstaben"
html_title:           "Haskell: Umwandlung eines Strings in Kleinbuchstaben"
simple_title:         "Umwandlung eines Strings in Kleinbuchstaben"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum
Es gibt viele Gründe, warum man in Haskell eine Zeichenkette (String) in Kleinbuchstaben umwandeln möchte. Ein häufiger Grund ist zum Beispiel die Vergleichbarkeit von Strings, da Groß- und Kleinschreibung oft nicht beachtet werden soll.

## Wie Geht's
Die einfachste Möglichkeit, eine Zeichenkette in Kleinbuchstaben umzuwandeln, ist die Verwendung der Funktion `toLower` aus dem `Data.Char` Modul. Diese Funktion akzeptiert einen einzelnen Buchstaben oder eine ganze Zeichenkette und gibt eine neue Zeichenkette zurück, in der alle Buchstaben in Kleinbuchstaben umgewandelt wurden. Hier ein Beispiel:

```Haskell
import Data.Char (toLower)

main = do
  putStrLn $ toLower "Hallo, WELt!"

-- Ausgabe: hallo, welt!
```

Um zu sehen, wie `toLower` tatsächlich funktioniert, können wir uns den Quellcode anschauen. Dabei werden wir einige Begriffe aus der Haskell Syntax erklären.

```Haskell
-- Die einfache Funktion toLower erwartet einen Char und gibt ebenfalls einen Char zurück.
toLower :: Char -> Char

-- Hier wird ein Pattern Matching verwendet, um die verschiedenen Fälle von Klein- und Großbuchstaben abzudecken.
toLower c
  | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32) :: Char
  | otherwise = c
```

In diesem Beispiel wird der Charakter `c` überprüft, ob es sich um einen Großbuchstaben handelt. Falls ja, wird mit `fromEnum` der Unicode-Wert des Buchstabens abgerufen und um 32 erhöht. Dieser Wert wird dann mit `toEnum` wieder in einen Charakter umgewandelt und zurückgegeben. Andernfalls wird einfach der original Wert von `c` zurückgegeben.

## Tiefer Tauchen
Die Funktion `toLower` ist zwar eine einfache Möglichkeit, um Zeichenketten in Kleinbuchstaben umzuwandeln, aber sie ist nicht die einzige. Es gibt auch komplexere Möglichkeiten, die je nach Anforderungen besser geeignet sein könnten. Zum Beispiel können wir auch die Funktionalität der Funktion `toLower` selbst implementieren, indem wir eine Kette von Funktionen anwenden.

```Haskell
-- Durch das Zusammenspiel mehrerer Funktionen können wir eine Zeichenkette in Kleinbuchstaben umwandeln.
lowerCase :: String -> String
lowerCase = map toLower
```

Hier sehen wir eine neue Sache in Haskell, die sogenannten "Kurzen Kompositionen", die durch `loweCase = map toLower` dargestellt werden. Das bedeutet einfach, dass die Funktion `map toLower` die Argumenten dieser Funktion ist. `map` ist eine polymorphe Funktion, die auf einer Liste von Elementen eine gegebene Funktion anwendet. In diesem Fall wenden wir `toLower` auf jeden einzelnen Charakter in der Zeichenkette an, was dazu führt, dass die gesamte Zeichenkette in Kleinbuchstaben umgewandelt wird. Diese Art der Verkettung von Funktionen ist ein wesentlicher Bestandteil der funktionalen Programmierung und ermöglicht es uns, komplexe Operationen auf einfache Weise zu definieren.

## Siehe Auch
- [Haskell Dokumentation](https://www.haskell.org/documentation/)
- [Haskell für Umsteiger](https://de.wikibooks.org/wiki/Haskell_f%C3%BCr_Umsteiger)
- [Einführung in die Funktionale Programmierung mit Haskell](https://www.youtube.com/watch?v=gBf8Btc6r_8) (Video)