---
title:                "Die Länge eines Strings finden"
html_title:           "Haskell: Die Länge eines Strings finden"
simple_title:         "Die Länge eines Strings finden"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum

Es gibt viele Gründe, warum man die Länge einer Zeichenkette in Haskell finden könnte. Vielleicht möchte man zum Beispiel überprüfen, ob eine Eingabe innerhalb eines bestimmten Bereichs liegt, oder man muss die Länge eines Textes für ein bestimmtes Formatierungssystem berechnen. Egal aus welchem Grund, die Länge zu kennen ist ein wichtiger Teil der Programmierung und kann in verschiedenen Situationen sehr nützlich sein.

## Wie geht es?

Die Länge einer Zeichenkette in Haskell zu finden, ist relativ einfach. Es gibt zwei verschiedene Methoden, die wir uns anschauen werden. Aber bevor wir damit beginnen, müssen wir zunächst verstehen, was eine Zeichenkette in Haskell ist. Im Gegensatz zu vielen anderen Programmiersprachen, in denen eine Zeichenkette eine Kette von einzelnen Zeichen ist, ist eine Zeichenkette in Haskell eine Liste von Zeichen. Wir können also die Funktion `length` verwenden, um die Länge einer Zeichenkette zu finden.

```
-- Beispiel 1
length "Hallo" 

-- Ausgabe: 5

-- Beispiel 2
length "12345"

-- Ausgabe: 5
```
Wie wir sehen können, gibt die `length` Funktion die Anzahl der Elemente in der Liste zurück, was der Länge der Zeichenkette entspricht.

Eine andere Möglichkeit, die Länge einer Zeichenkette in Haskell zu finden, ist die Verwendung von `genericLength`. Diese Funktion ist ähnlich wie `length`, jedoch arbeitet sie mit allen Arten von Zahlen, nicht nur mit Ganzzahlen.

```
-- Beispiel 
genericLength "Hallo"

-- Ausgabe: 5.0 (aufgrund der Verwendung von Dezimalstellen, um beliebige Zahlen zu unterstützen)
```

## Tiefer Einblick

Um besser zu verstehen, wie die `length` Funktion funktioniert, sollten wir uns ihre Implementierung ansehen. In erster Linie ist es wichtig zu wissen, dass die Funktion `length` eine polymorphe Funktion ist, was bedeutet, dass sie mit verschiedenen Datentypen funktioniert. Ihre Typsignatur ist `length :: Foldable t => t a -> Int`, was bedeutet, dass sie eine Liste von Elementen des Datentyps `a` empfängt und eine Ganzzahl zurückgibt.

Die `length` Funktion ist so definiert, dass sie die `null` Funktion verwendet, um zu überprüfen, ob die Liste leer ist. Wenn dies der Fall ist, gibt sie 0 zurück. Wenn die Liste nicht leer ist, verwendet sie die Funktion `foldr` (ein weiteres wichtiges Haskell-Konzept), um eine rekursive Liste zu durchlaufen und dabei jedes Element zu zählen. Auf diese Weise kann die `length` Funktion unabhängig von der Länge der Liste effizient arbeiten.

## Siehe auch

- [Haskell-Dokumentation zu `length`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html#v:length)
- [Einführung in Haskell für Anfänger](https://www.freecodecamp.org/news/learn-functional-programming-with-haskell-beginners-guide/)
- [Weitere Methoden zum Arbeiten mit Zeichenketten in Haskell](https://wiki.haskell.org/Strings)