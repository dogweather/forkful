---
title:                "Gleam: Vergleich von zwei Datumsangaben"
simple_title:         "Vergleich von zwei Datumsangaben"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum
Vergleichen von zwei Daten in der Programmierung ist ein wichtiger Schritt bei der Datumsmanipulation. Es ermöglicht uns, bestimmte Aktionen basierend auf dem Verhältnis zwischen zwei Daten auszuführen. Wenn Sie zum Beispiel überprüfen möchten, ob eine Sitzung abgelaufen ist, müssen Sie das aktuelle Datum mit dem Ablaufdatum vergleichen.

## How To
Um zwei Daten in Gleam zu vergleichen, verwenden wir die `cmp` Funktion. Diese Funktion vergleicht zwei Daten und gibt `Ordering` zurück, je nachdem ob das erste Datum vor, nach oder gleich dem zweiten Datum liegt.

```Gleam
let datum_1 = Date.from_iso8601("2021-04-08")
let datum_2 = Date.from_iso8601("2021-04-10")

// Vergleichen der Daten und Zuweisen der Ergebnisse an eine Variable
let vergleich = Date.cmp(datum_1, datum_2)

// Ausgabe des Vergleichs mit einem einfachem Match-Ausdruck
case vergleich {
  Ordering.Lt -> IO.print("Das erste Datum ist vor dem zweiten Datum")
  Ordering.Gt -> IO.print("Das erste Datum ist nach dem zweiten Datum")
  Ordering.Eq -> IO.print("Beide Daten sind gleich")
}
```

Die möglichen Ausgaben wären je nachdem welches Datum als erstes eingegeben wurde:
- `Das erste Datum ist vor dem zweiten Datum`
- `Das erste Datum ist nach dem zweiten Datum`
- `Beide Daten sind gleich`

## Deep Dive
Die `cmp` Funktion verwendet den Vergleichsoperator `<`intern um die Reihenfolge der Daten zu bestimmen. Dies bedeutet, dass beide Daten zu `Integers` konvertiert und dann verglichen werden. Dabei entspricht jeder Tag in einem `Date`-Objekt einem `Integer` und je größer der Tag, desto größer der `Integer`-Wert.

Bei der Verwendung von `cmp` müssen beide Daten vom gleichen Typ sein. Andernfalls erhält man einen Compilerfehler.

## Siehe Auch
- [Date Module Documentation](https://gleam.run/core/date.html)
- [Comparison Operators in Gleam](https://gleam.run/book/stdlib.html#comparison-operators)