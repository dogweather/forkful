---
title:    "Elm: Vergleich von zwei Daten."
keywords: ["Elm"]
---

{{< edit_this_page >}}

# Warum

Vergleichen von zwei Daten kann sehr wichtig in der Elm-Programmierung sein. Es kann dazu beitragen, zu überprüfen, ob ein Datum in der Vergangenheit oder Zukunft liegt, oder ob ein Zeitintervall abgelaufen ist. In diesem Blogbeitrag werden wir uns genauer ansehen, wie man in Elm zwei Daten vergleichen kann.

## Wie geht das?

Um zwei Daten in Elm zu vergleichen, können wir die integrierten Vergleichsoperatoren verwenden. Zum Beispiel können wir den Operator `>` verwenden, um zu überprüfen, ob eine Datum später als ein anderes ist.

```
elm date1 > date2
```

Wenn date1 später als date2 ist, wird dies `True` zurückgeben, andernfalls `False`. Genauso können wir auch den `<=` Operator verwenden, um zu überprüfen, ob ein Datum vor oder gleich einem anderen liegt.

```
elm date1 <= date2
```

Im folgenden Beispiel werden wir zwei Daten vergleichen und die Ausgabe basierend auf dem Ergebnis des Vergleichs anzeigen.

```
elm import Date exposing (Date)

date1: Date
date1 =
    Date.fromCalendarDate 2021 1 1

date2: Date
date2 =
    Date.fromCalendarDate 2020 1 1

output: String
output =
    if date1 > date2 then
        "Datum 1 ist später als Datum 2"
    else if date1 < date2 then
        "Datum 1 ist früher als Datum 2"
    else
        "Beide Daten sind gleich"

elm output
```

Die Ausgabe dieses Codes wird "Datum 1 ist später als Datum 2" sein.

## Tiefere Einblicke

Bei der Verwendung von Vergleichsoperatoren ist es wichtig zu beachten, dass sie in erster Linie auf Basis der Zeitzone vergleichen. Wenn wir jedoch genauer unter die Haube schauen, werden wir feststellen, dass sie tatsächlich auf der Anzahl der Millisekunden seit dem 1. Januar 1970 basieren.

Dies kann zu unerwarteten Ergebnissen führen, wenn Datumswerte mit unterschiedlichen Zeitzonen verglichen werden. In solchen Fällen ist es möglicherweise besser, die Funktion `Date.compare` zu verwenden, die auf der tatsächlichen Reihenfolge der Daten basiert, unabhängig von der Zeitzone.

```
import Date exposing (Date)

date1: Date
date1 =
    Date.fromCalendarDate 2021 1 1

date2: Date
date2 =
    Date.fromCalendarDate 2020 12 31

output: String
output =
    case Date.compare date1 date2 of
        LT ->
            "Datum 1 liegt vor Datum 2"
        EQ ->
            "Beide Daten sind gleich"
        GT ->
            "Datum 1 liegt nach Datum 2"

elm output
```

Die Ausgabe hier wird "Datum 1 liegt nach Datum 2" sein, da tatsächlich das Jahr 2021 später ist als das Jahr 2020.

## Siehe auch

* Die Elm-Dokumentation zu [Date](https://package.elm-lang.org/packages/elm/core/latest/Date)
* Das [Date-Extra-Paket](https://package.elm-lang.org/packages/justinmimbs/date-extra/latest/) für erweiterte Datumsmöglichkeiten