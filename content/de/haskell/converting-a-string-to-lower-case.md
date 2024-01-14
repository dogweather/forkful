---
title:    "Haskell: Umwandlung eines Strings in Kleinbuchstaben"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Warum

Eine gängige Aufgabe beim Programmieren ist es, eine Zeichenfolge in Kleinbuchstaben umzuwandeln. In diesem Blogbeitrag erfahren Sie, wie Sie dies mit Hilfe der funktionalen Programmiersprache Haskell ganz einfach umsetzen können.

## Wie geht's

Um eine Zeichenfolge in Haskell in Kleinbuchstaben umzuwandeln, gibt es verschiedene Ansätze.

Ein möglicher Weg ist die Verwendung der Funktion `map` in Kombination mit der Unterfunktion `toLower` aus dem Modul `Data.Char`. Diese Funktionen sind sehr hilfreich, da sie bereits in der Standardbibliothek von Haskell vorhanden sind und somit keine zusätzlichen Imports benötigen.

```Haskell
import Data.Char

-- Definition der Funktion zur Umwandlung in Kleinbuchstaben
toLowerString :: String -> String
toLowerString str = map toLower str
```

Der obige Code zeigt eine einfache Definition der Funktion `toLowerString`, die eine Zeichenfolge als Eingabe erwartet und diese mithilfe von `map` in Kleinbuchstaben umwandelt.

Ein weiterer Ansatz ist die Verwendung von Pattern Matching. Hierbei wird geprüft, ob die übergebene Zeichenfolge leer ist und falls ja, wird sie direkt zurückgegeben. Ansonsten wird der erste Buchstabe in Kleinbuchstaben umgewandelt und der restliche Teil der Zeichenfolge rekursiv bearbeitet.

```Haskell
-- Pattern Matching Funktion zum Umwandeln in Kleinbuchstaben
toLowerStringMatch :: String -> String
toLowerStringMatch "" = ""
toLowerStringMatch (x:xs) = toLower x : toLowerString xs
```

Beide Ansätze erzielen das gleiche Ergebnis. Hier ein Beispiel mit der Eingabe "HaSkElL" und der erwarteten Ausgabe "haskell".

```Haskell
-- Output
toLowerString "HaSkElL" -- "haskell"
toLowerStringMatch "HaSkElL" -- "haskell"
```

## Tief tauchen

Der Vorteil des zweiten Ansatzes liegt in der besseren Performance, da keine spezifische Funktion wie `map` verwendet wird. Jedoch ist es auch möglich, die Funktion in einer Art und Weise zu definieren, die interne Optimierungen ermöglicht.

Eine Möglichkeit ist die Verwendung von `foldl`, um die Funktion effizienter zu machen. Hierbei wird die Zeichenfolge von links nach rechts durchlaufen und bei jedem Schritt wird der aktuelle Buchstabe in Kleinbuchstaben umgewandelt und an den zuvor durchlaufenen Teil der Zeichenfolge angehängt.

```Haskell
-- Effizientere Implementation mit foldl
toLowerStringFold :: String -> String
toLowerStringFold = foldl (\acc x -> acc ++ [toLower x]) ""
```

Diese Implementation ist noch robuster und schneller, da sie nicht die gesamte Zeichenfolge neu erstellen muss.

## Siehe auch

- Offizielle Haskell Dokumentation: https://www.haskell.org/documentation/
- "Real World Haskell" Buch: http://book.realworldhaskell.org/read/
- "Learn You a Haskell for Great Good" online Buch: http://learnyouahaskell.com/