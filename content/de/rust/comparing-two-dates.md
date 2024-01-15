---
title:                "Vergleich von zwei Datumsangaben"
html_title:           "Rust: Vergleich von zwei Datumsangaben"
simple_title:         "Vergleich von zwei Datumsangaben"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum

Sich mit der Vergleichung von zwei Datumswerten zu beschäftigen mag zunächst unbedeutend erscheinen, aber es ist eine sehr wichtige Fähigkeit in der Programmierung. Ob es darum geht zu überprüfen, ob eine bestimmte Zeitspanne verstrichen ist oder die Reihenfolge von Ereignissen zu bestimmen, das Vergleichen von Daten ist ein grundlegendes Konzept in der Datumsmanipulation.

## So geht's

Um zwei Datumsangaben in Rust zu vergleichen, können wir die Methode `cmp()` verwenden, die auf Objekten implementiert ist, die das Trait `Ord` implementieren. Diese Methode gibt ein `Ordering`-Enum zurück, das entweder `Less`, `Equal` oder `Greater` sein kann, je nachdem, welches Datum früher ist. Schauen wir uns dazu ein Beispiel an:

```rust
let date1 = NaiveDate::from_ymd(2020, 1, 1); //1. Januar 2020
let date2 = NaiveDate::from_ymd(2021, 3, 30); //30. März 2021

let order = date1.cmp(&date2);

match order {
    Ordering::Less => println!("Das erste Datum liegt vor dem zweiten Datum."),
    Ordering::Equal => println!("Beide Daten sind identisch."),
    Ordering::Greater => println!("Das zweite Datum liegt vor dem ersten Datum."),
}
```

Die Ausgabe wird `Das erste Datum liegt vor dem zweiten Datum.` sein, da `date1` früher als `date2` ist. Beachte, dass wir `date2` als Referenz übergeben müssen, da die `cmp()`-Methode eine Referenz erwartet.

## In die Tiefe

Der Vergleich von zwei Datumswerten mag auf den ersten Blick einfach erscheinen, aber es gibt einige Fallstricke zu beachten. Zunächst müssen die beiden Datumsangaben im gleichen Format sein, um richtig verglichen werden zu können. Wenn wir in unserem obigen Beispiel `date2` als `NaiveDate::from_ymd(21, 3, 30)` definieren (statt `2021`), wird die Ausgabe `Das zweite Datum liegt vor dem ersten Datum.` sein, da Rust standardmäßig zweistellige Jahre als 20xx interpretiert.

Außerdem müssen wir bedenken, dass Datumsangaben in unterschiedlichen Zeitzonen verschieden interpretiert werden können. Um dies zu vermeiden, können wir das Modul `DateTime` aus dem Paket `chrono` verwenden, das es ermöglicht, Zeitzonen explizit anzugeben.

```rust
let date1 = DateTime::parse_from_rfc3339("2020-01-01T00:00:00+00:00").unwrap(); //1. Januar 2020 in UTC-Zeit
let date2 = DateTime::parse_from_rfc3339("2020-01-01T01:00:00+01:00").unwrap(); //1. Januar 2020 in Mitteleuropäischer Zeit (MEZ)

let order = date1.cmp(&date2);

match order {
    Ordering::Less => println!("Das erste Datum liegt vor dem zweiten Datum."),
    Ordering::Equal => println!("Beide Daten sind identisch."),
    Ordering::Greater => println!("Das zweite Datum liegt vor dem ersten Datum."),
}
```

In diesem Beispiel werden die beiden Datumsangaben als unterschiedlich interpretiert, da sie in verschiedenen Zeitzonen liegen.

## Siehe auch

- [Rust-Dokumentation zu Datum und Zeit](https://doc.rust-lang.org/std/time/index.html)
- [Chrono-Dokumentation](https://docs.rs/chrono/0.4.9/chrono/index.html)