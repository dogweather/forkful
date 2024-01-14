---
title:    "Haskell: String in Großbuchstaben"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Warum

Haskell ist eine fortschrittliche Programmiersprache, die viele einzigartige Funktionen bietet, darunter die Möglichkeit, einen String zu verändern, indem man ihn in Großbuchstaben umwandelt. Warum sollte man diese Funktion nutzen? Nun, man könnte zum Beispiel einen Namen in einem Format wie "Vorname Nachname" in "VORNAME NACHNAME" ändern wollen. Oder man könnte einfach Spaß daran haben, Wörter in Großbuchstaben zu sehen!

## So geht's

Um einen String in Haskell in Großbuchstaben umzuwandeln, gibt es mehrere Möglichkeiten. Hier sind zwei Beispiele:

```
-- Beispiel 1: Mit der Funktion "map"
map toUpper "hallo" -- Output: "HALLO"

-- Beispiel 2: Mit einem "case" Statement
capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = [toUpper x] ++ xs
-- Beispielaufruf: capitalize "hallo" -- Output: "HALLO"
```

Das erste Beispiel nutzt die bereits eingebaute Funktion `toUpper` und die Funktion `map`, um jeden Buchstaben in einem String in Großbuchstaben umzuwandeln. Das zweite Beispiel zeigt eine selbstgeschriebene Funktion, die einen String nimmt und den ersten Buchstaben in einen Großbuchstaben umwandelt, während alle anderen Buchstaben gleich bleiben.

## Tiefergehend

In Haskell werden Strings als Listen von Zeichen dargestellt. Deshalb funktioniert das Beispiel mit `map` so gut - es wendet einfach die Funktion `toUpper` auf jedes Zeichen im String an. Eine weitere interessante Funktion für die Arbeit mit Strings ist `words`, die einen String in eine Liste von Wörtern aufteilt. So könnte man zum Beispiel eine Funktion schreiben, die einen Namen im Format "Vorname Nachname" nimmt und in "Nachname, Vorname" umwandelt:

```
switchName :: String -> String
switchName name = let lastName = last (words name)
                      firstName = head (words name)
                  in lastName ++ ", " ++ firstName
-- Beispielaufruf: switchName "Max Mustermann" -- Output: "Mustermann, Max"
```

## Siehe auch

- [Haskell Dokumentation zu Strings](https://www.haskell.org/tutorial/strings.html)
- [Haskell Funktionen für Listen](https://www.haskell.org/tutorial/strings.html)