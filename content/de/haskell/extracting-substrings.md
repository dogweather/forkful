---
title:                "Unterzeichenketten extrahieren"
html_title:           "Haskell: Unterzeichenketten extrahieren"
simple_title:         "Unterzeichenketten extrahieren"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum
Warum sollte man sich damit beschäftigen, Teilstrings aus anderen Strings zu extrahieren? Ganz einfach: Weil es in vielen Fällen eine nützliche Funktion ist, um bestimmte Informationen aus einem längeren String zu isolieren. Zum Beispiel können wir so aus einer E-Mail-Adresse den Benutzernamen oder die Domain herausfiltern.

## How To

Um Teilstrings in Haskell zu extrahieren, können wir die Funktion `take` und `drop` verwenden. Hier ein Beispiel:

```Haskell
-- Eine Funktion, die aus einem String eine Liste von Teilstrings mit einer bestimmten Länge erstellt
teilstrings :: Int -> String -> [String]
teilstrings n str
  | length str < n = [] -- Wenn der String kürzer ist als die gewünschte Länge, wird eine leere Liste zurückgegeben
  | otherwise = take n str : teilstrings n (drop 1 str) -- Ansonsten wird der erste Teilstring mit Länge n extrahiert und der restliche String wird rekursiv weiterverarbeitet

-- Beispielaufruf mit Ausgabe
teilstrings 3 "Haskell ist eine funktionale Programmiersprache"
-- ["Has", "ask", "ske", "kel", "ell", "ll ", "l i", " in", "ist", "st ", "t e", " ei", "ein", "ine", "ne ", "e f", " fu", "funk", "unk", "nkt", "kti", "tio", "ion", "ona", "nal", "ale", "le ", "e P", " Pr", "Pro", "rog", "ogr", "gra"]
```

In diesem Beispiel wird die Funktion `teilstrings` definiert, die eine bestimmte Anzahl von Teilstrings mit der Länge `n` aus dem String `str` erstellt. Dazu wird die Funktion `take` verwendet, die die ersten `n` Elemente aus einer Liste zurückgibt, sowie die Funktion `drop`, die die ersten `n` Elemente aus einer Liste entfernt und den Rest zurückgibt. Mithilfe von Rekursion wird so der Rest des Strings weiterverarbeitet, bis keine Teilstrings mehr extrahiert werden können.

## Deep Dive

In Haskell gibt es noch weitere Funktionen und Techniken, um Teilstrings zu extrahieren. Zum Beispiel kann man mithilfe der Funktion `splitAt` einen String an einer bestimmten Position teilen und so den ersten oder letzten Teilstring extrahieren. Auch mit regulären Ausdrücken kann man sehr präzise Teilstrings auswählen.

Eine weitere nützliche Funktion ist `takeWhile`, die eine Liste von Elementen ausgibt, solange eine bestimmte Eigenschaft erfüllt ist. So könnte man zum Beispiel alle Großbuchstaben am Anfang des Strings extrahieren:

```Haskell
teilstringGroß :: String -> String
teilstrin