---
title:                "Vergleich von zwei Daten"
html_title:           "Gleam: Vergleich von zwei Daten"
simple_title:         "Vergleich von zwei Daten"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum

Vergleichen von zwei Daten ist eine Möglichkeit, um festzustellen, welches Datum früher oder später ist. Diese Funktion kann nützlich sein, um z.B. zu überprüfen, ob ein Termin schon vorbei ist oder ob ein Ereignis in der Zukunft liegt.

## Wie geht's

Der einfachste Weg, um zwei Daten zu vergleichen, ist die Verwendung der `compare_dates`-Funktion in Gleam. Schauen wir uns ein Beispiel an:

```Gleam
let date_a = Date.create(2020, 10, 15)
let date_b = Date.create(2020, 8, 30)
let comparison = Date.compare_dates(date_a, date_b)
```

In diesem Beispiel haben wir zwei Daten `date_a` und `date_b` erstellt und dann die `compare_dates`-Funktion verwendet, um sie zu vergleichen. Die Funktion gibt `greater` zurück, wenn `date_a` später ist als `date_b`, `less` wenn `date_b` später ist als `date_a` und `equal` wenn beide Daten gleich sind.

Die Ausgabe von `comparison` ist `greater`, da `date_a` später im Jahr liegt als `date_b`.

Um sicherzustellen, dass die Daten wirklich gleich sind, können wir auch die `equal_dates`-Funktion verwenden:

```Gleam
let date_c = Date.create(2020, 10, 15)
let comparison = Date.equal_dates(date_a, date_c)
```

In diesem Beispiel wird `comparison` als `true` zurückgegeben, da beide Daten am selben Tag, im selben Monat und im selben Jahr erstellt wurden.

## Tiefere Einblicke

Beim Vergleichen von Daten muss man sich bewusst sein, dass die Zeitkomponente ebenfalls berücksichtigt wird. Das bedeutet, dass zwei Daten mit derselben Uhrzeit als gleich angesehen werden, auch wenn sie an unterschiedlichen Tagen oder in unterschiedlichen Monaten liegen.

Es ist auch wichtig zu wissen, dass die `compare_dates`-Funktion Daten mit unterschiedlicher Genauigkeit vergleichen kann. Zum Beispiel können wir eine Zeitangabe angeben, während die anderen Daten nur Jahr, Monat und Tag enthalten:

```Gleam
let date_d = Date.create(2020, 10, 15, (3, 30, 0, 0))
let date_e = Date.create(2020, 10, 15)
let comparison = Date.compare_dates(date_d, date_e)
```

In diesem Beispiel wird `0` zurückgegeben, da die Zeitkomponente bei beiden Daten gleich ist. Wenn wir jedoch die `equal_dates`-Funktion verwenden, wird immer noch `true` zurückgegeben, da die Daten am selben Tag, im selben Monat und im selben Jahr erstellt wurden.

## Siehe auch

Weitere Informationen zum Vergleichen von Daten und anderen Gleam-Funktionen finden Sie in der offiziellen Dokumentation:

- [Date Module](https://gleam.run/modules/stdlib/Date.html)
- [Official Gleam Documentation](https://gleam.run/)