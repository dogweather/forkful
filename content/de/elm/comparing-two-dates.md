---
title:                "Zwei Daten vergleichen"
html_title:           "Elm: Zwei Daten vergleichen"
simple_title:         "Zwei Daten vergleichen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum

Das Vergleichen von zwei Daten kann in vielen Situationen nützlich sein, besonders in der Entwicklung von Webanwendungen. Es hilft dabei, festzustellen, ob ein Datum früher, später oder gleichzeitig wie ein anderes Datum ist.

## How To

Das Vergleichen von zwei Daten ist in Elm sehr einfach. Zunächst müssen die beiden Daten als `Date`-Typen definiert werden. Dann kann die Funktion `compare` verwendet werden, um die Relation zwischen den beiden Daten zu bestimmen.

```Elm
import Date exposing (..)

date1 : Date
date1 = fromCalendarDate 2020 6 15 -- 15. Juni 2020

date2 : Date
date2 = fromCalendarDate 2020 6 20 -- 20. Juni 2020

compareDates : Date -> Date -> String
compareDates date1 date2 =
    case compare date1 date2 of
        LT -> "date1 ist früher als date2"
        EQ -> "date1 und date2 sind gleich"
        GT -> "date1 ist später als date2"

compareDates date1 date2 -- "date1 ist früher als date2"
```

Die Funktion `compare` liefert entweder `LT` (Less Than), `EQ` (Equal) oder `GT` (Greater Than) zurück, je nachdem, welcher der beiden Daten früher, später oder gleichzeitig ist. Anschließend kann dies in einem `case`-Ausdruck verarbeitet werden.

## Deep Dive

Beim Vergleichen von zwei Daten gibt es einige Dinge zu beachten. Zum Beispiel funktioniert die `compare`-Funktion nur für Daten des Typs `Date`. Wenn eine andere Datenstruktur verwendet wird, muss möglicherweise eine andere Methode zum Vergleichen implementiert werden.

Außerdem gilt es zu beachten, dass die Zeitzone bei der Definition der Daten eine Rolle spielt. So kann es sein, dass zwei Daten, die in unterschiedlichen Zeitzonen definiert wurden, dennoch als gleich angesehen werden, obwohl sie in verschiedenen Zeitpunkten stattfinden.

Es gibt auch weitere Funktionen wie `isBefore`, `isAfter` und `isSame` in der `Date`-Modul, die dabei helfen können, verschiedene Aspekte der Daten zu vergleichen.

## Siehe auch

- [Offizielle Dokumentation zu Datum und Zeit in Elm](https://package.elm-lang.org/packages/elm/time/latest/)
- [Ein Tutorial zur Arbeit mit Datum und Zeit in Elm](https://guide.elm-lang.org/datetime/)