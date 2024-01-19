---
title:                "Teilzeichenketten extrahieren"
html_title:           "PowerShell: Teilzeichenketten extrahieren"
simple_title:         "Teilzeichenketten extrahieren"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

# Arbeiten mit Teilstrings in Haskell: Ein kurzer Überblick

## Was & Warum?

Teilstrings, auch bekannt als "Substrings", sind kleinere Teile von größeren Zeichenketten. In der Programmierung verwenden wir Teilstrings oft, um bestimmte Informationen aus größeren Datenblöcken zu extrahieren oder zu manipulieren. 

## Wie geht das:

Hier ist ein einfacher Code-Snippet für die Extraktion von Teilstrings in Haskell. Unter Verwendung der Funktion 'take'.

```Haskell
-- Nimmt die ersten n Zeichen einer Zeichenkette s
substringStart :: Int -> String -> String
substringStart n s = take n s

ghci> substringStart 5 "Ein einfacher Haskell Code"
"Ein e"
```

Und hier verwenden wir die Funktionen 'drop' und 'take', um eine Substring aus der Mitte zu extrahieren.

```Haskell
-- Nimmt n Zeichen einer Zeichenkette s, beginnend bei Index i
substringMitte :: Int -> Int -> String -> String
substringMitte i n s = take n (drop i s)

ghci> substringMitte 4 6 "Ein einfacher Haskell Code"
"einfac"
```

## Vertiefung

Obwohl die Funktionen 'drop' und 'take' in Haskell weit verbreitet sind und oft verwendet werden, um Substrings zu extrahieren, gibt es auch andere Werkzeuge und Funktionen wie 'splitAt', die ähnliche Aufgaben erfüllen können.

Historisch gesehen ermöglichte Haskell-Funktionalität wie diese durch das Entwerfen dynamischer Funktionen und sogar Operatoren, um in Zeichenketten nach Bedarf zu graben. Haskell hat eine starke Funktionale Programmierbegründung, die die Notwendigkeit betont, Daten zu transformieren, anstatt ihren Zustand zu verändern. Diese Philosophie spiegelt sich in der Art und Weise wider, wie Substrings extrahiert und manipuliert werden.

In Bezug auf die Implementierungsdetails: 'take' arbeitet durch rekursive Aufrufe, wobei jedes Charakter schrittweise hinzugefügt wird; ein Muster, das üblicherweise in der funktionalen Programmierung verwendet wird. 'drop' funktioniert auf ähnliche Weise, überspringt jedoch die ersten n-Zeichen, bevor sie den Rest der Zeichenkette zurückgibt. 

## Siehe auch

Weitere Informationen zur Arbeit mit Substrings in Haskell finden Sie unter folgenden Links:

1. Learn You a Haskell for Great Good: [Strings](http://learnyouahaskell.com/starting-out#an-intro-to-lists)
2. Real World Haskell: [Working with Lists and Strings](http://book.realworldhaskell.org/read/functional-programming.html)
3. Haskell Wiki: [string manipulation](https://wiki.haskell.org/index.php?title=String_manipulation&oldid=60805)