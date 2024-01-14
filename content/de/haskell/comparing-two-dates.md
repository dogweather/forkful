---
title:                "Haskell: Vergleich von zwei Daten"
programming_language: "Haskell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum

In der Programmierung gibt es oft Situationen, in denen wir zwei verschiedene Daten vergleichen müssen. Zum Beispiel wenn wir prüfen wollen, ob ein bestimmtes Datum in der Zukunft oder Vergangenheit liegt oder wenn wir zwei Ereignisse zeitlich miteinander vergleichen möchten. In diesem Blogpost werden wir uns anschauen, wie wir in Haskell zwei Daten vergleichen können.

## Wie man zwei Daten in Haskell vergleicht

Um zwei Daten in Haskell zu vergleichen, können wir die Funktion `compare` verwenden, die Teil der Standardbibliothek ist. Diese Funktion nimmt zwei Argumente vom selben Typ und gibt als Ergebnis eine `Ordering`-Value zurück, also entweder `LT` (kleiner), `EQ` (gleich) oder `GT` (größer). Schauen wir uns ein Beispiel an:

```Haskell
-- Vergleich von Tagen
compareDay :: Day -> Day -> Ordering
compareDay day1 day2 = compare day1 day2

-- Beispielaufruf
-- 11. Mai 2021 und 25. Dezember 2021
compareDay (fromGregorian 2021 5 11) (fromGregorian 2021 12 25)
-- Output: LT
```

In diesem Beispiel vergleichen wir zwei `Day`-Werte, die wir mit der Funktion `fromGregorian` aus dem Modul `Data.Time.Calendar` erstellen. Wir sehen, dass der Output `LT` ist, da der 11. Mai vor dem 25. Dezember liegt.

Neben `Day`-Werten können wir auch andere Datentypen vergleichen, solange sie eine Instanz der Typklasse `Ord` sind. Das bedeutet, dass sie eine strikte totale Ordnung implementieren können.

## Tiefergehende Information

Die `compare`-Funktion nutzt die `Ord`-Instanz des zu vergleichenden Typs, um zu bestimmen, wie die Werte zueinander stehen. Bei `Day`-Werten wird zum Beispiel das Datum auf eine Zahl kodiert und dann verglichen. Aber auch bei anderen Datentypen kann die Implementierung von `Ord` je nach Typ unterschiedlich sein.

Ein wichtiger Punkt beim Vergleichen von Daten ist auch, dass die Werte vom selben Typ sein müssen. Wenn wir versuchen, eine `String`-Value mit einer `Float`-Value zu vergleichen, bekommen wir einen Fehler, da diese Werte nicht vom selben Typ sind. Es ist daher wichtig, darauf zu achten, dass wir bei unserer Implementierung immer mit dem gleichen Typ von Werten arbeiten.

## Siehe auch

- [Haskell Wikibook - Ord](https://en.wikibooks.org/wiki/Haskell/Ord)
- [Haskell Docs - compare function](https://hackage.haskell.org/package/base-4.15.1.0/docs/Prelude.html#v:compare)