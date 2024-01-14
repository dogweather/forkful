---
title:                "Haskell: Vergleich von zwei Datum"
simple_title:         "Vergleich von zwei Datum"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum

Vergleich von zwei Daten? Warum sollte man das überhaupt tun? Nun, es gibt viele Anwendungen, bei denen man Datumsangaben vergleichen muss. Zum Beispiel in der Buchhaltung, bei der Verwaltung von Aufgaben oder Terminen, und sogar in der täglichen Planung. Daher ist es wichtig zu wissen, wie man zwei Daten in Haskell vergleichen kann.

## Wie Geht's

Ein Vergleich von zwei Daten in Haskell kann auf verschiedene Arten durchgeführt werden. Eine Möglichkeit ist die Verwendung der Standardfunktion `compare`, die zwei Werte jedes Ordertyps vergleichen kann, einschließlich Daten.

Das folgende Beispiel zeigt, wie man die `compare`-Funktion verwendet, um zwei Daten zu vergleichen und das Ergebnis als `Ordering`-Wert zurückzugeben:

```Haskell
-- Vergleich von zwei Daten
compareDates :: Date -> Date -> Ordering
compareDates date1 date2 = compare date1 date2
```

Die Rückgabewerte von `compare` können entweder `LT` (geringer), `GT` (größer) oder `EQ` (gleich) sein. Hier ist ein Beispiel für den Vergleich von zwei verschiedenen Daten mit dem obigen Code:

```Haskell
-- Beispiel für den Vergleich von Daten
>>> compareDates (Date 2020 1 1) (Date 2020 1 2)
LT
```

In diesem Fall ist das erste Datum (1. Januar 2020) kleiner als das zweite Datum (2. Januar 2020).

## Tiefer Gehen

Für einen genaueren Blick auf die Vergleichsfunktion von Daten in Haskell, kann man sich die Unterstützungsfunktionen `(<)` (kleiner als), `(>)` (größer als) und `(==)` (gleich) ansehen. Diese unterstützen den `compare`-Operator und ermöglichen es uns, die Definition der Vergleichsfunktion für Daten zu ändern. Zum Beispiel können wir die Datumsstruktur in eine Instanz der `Ord`-Klasse umwandeln und die Vergleichsfunktion basierend auf dem Julianischen Kalender definieren.

```Haskell
-- Neue Definition von Date als Instanz von Ord
instance Ord Date where
  compare (Date y1 m1 d1) (Date y2 m2 d2)
    | y1 > y2 = GT
    | y1 < y2 = LT
    | m1 > m2 = GT
    | m1 < m2 = LT
    | d1 > d2 = GT
    | d1 < d2 = LT
    | otherwise = EQ
```

Durch diese neue Definition werden zwei Daten nun basierend auf dem Jahr, dem Monat und dem Tag verglichen, anstatt auf der standardmäßigen lexikalischen Reihenfolge.

## Siehe Auch

- [Offizielle Dokumentation zu Daten in Haskell](https://www.haskell.org/onlinereport/lexemes.html#sect2.6)
- [Weitere Beispiele und Erklärungen für die Verwendung von `compare` in Haskell](https://wiki.haskell.org/Ord)
- [Informationen über den Julianischen Kalender](https://de.wikipedia.org/wiki/Julianischer_Kalender)