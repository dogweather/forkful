---
title:                "Elm: Vergleich von zwei Daten"
simple_title:         "Vergleich von zwei Daten"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum

Das Vergleichen von zwei Daten kann in der Programmierung oft eine wichtige Aufgabe sein. Es ermöglicht es uns, bestimmte Aktionen basierend auf dem Datum auszuführen oder zu bestimmen, welches von zwei Daten das neueste ist.

## Wie man es macht

Es gibt verschiedene Möglichkeiten, um in Elm zwei Daten zu vergleichen. Eine einfache Möglichkeit besteht darin, die Funktion`Date.compare` zu verwenden. Hier ist ein Beispiel, um zu überprüfen, ob ein Datum vor einem anderen Datum liegt:

```Elm
firstDate : Date
firstDate =
    Date.fromYearMonthDay 2021 05 15

secondDate : Date
secondDate =
    Date.fromYearMonthDay 2021 06 01

Date.compare firstDate secondDate == Date.Less
```

In diesem Beispiel werden die Variablen `firstDate`und `secondDate`erstellt und mithilfe von `Date.fromYearMonthDay` mit einem konkreten Datum belegt. Dann wird die Funktion `Date.compare` aufgerufen und die beiden Daten als Argumente übergeben. Das Ergebnis wird mit der Konstanten `Date.Less` verglichen, die angibt, dass das erste Datum vor dem zweiten Datum liegt.

Es ist auch möglich, Daten anhand ihrer numerischen Werte zu vergleichen. Hierbei ist jedoch zu beachten, dass das Datum im Format `JJJJ-MM-TT` vorliegen muss. Zum Beispiel:

```Elm
Date.fromString "2021-05-15" == Date.fromString "2021-06-01"

> True
```

## Tiefergehende Informationen

Um eine genauere Vergleichsmöglichkeit zu haben, ist es hilfreich, die Funktion`Data.Monkey`zu verwenden. Diese Funktion bietet die Möglichkeit, zwei Daten miteinander zu addieren oder subtrahieren und das Resultat als Anzahl der Tage, Monate oder Jahre auszugeben. Dadurch ist es möglich, den genauen Unterschied zwischen zwei Daten zu ermitteln und basierend darauf eine Entscheidung zu treffen.

## Siehe auch

- [Date.compare Documentation](https://package.elm-lang.org/packages/elm/time/latest/Date#compare)
- [Data.Monkey Documentation](https://package.elm-lang.org/packages/elm/core/latest/Data-Monkey)