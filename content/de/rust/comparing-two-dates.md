---
title:    "Rust: Vergleichen von zwei Datumsangaben"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Warum

Das Vergleichen von zwei Datumswerten ist eine häufige Aufgabe in der Programmierung. Es ermöglicht uns, zu überprüfen, ob ein Datum vor oder nach einem bestimmten Datum liegt oder ob zwei Termine gleich sind. Dies kann nützlich sein, um beispielsweise Fälligkeitsdaten zu überprüfen oder um Zeitintervalle zu berechnen.

## Wie man zwei Daten in Rust vergleicht

Die Vergleichsoperationen in Rust sind mithilfe von Operatoren wie `<`, `>`, `<=` und `>=` möglich. Hier ist ein Beispiel, wie man zwei Datumswerte mit dem `==` Operator vergleichen könnte, um zu überprüfen, ob sie identisch sind:

```Rust
use chrono::{NaiveDate};

let date1 = NaiveDate::from_ymd(2021, 6, 1); // 1. Juni 2021
let date2 = NaiveDate::from_ymd(2021, 6, 1); // 1. Juni 2021

if date1 == date2 {
    println!("Die beiden Termine sind identisch.");
}
```

Wenn wir jedoch prüfen wollen, ob ein Datum vor oder nach einem anderen Datum liegt, können wir die anderen Operatoren verwenden. Hier ist ein Beispiel, um zu überprüfen, ob `date1` vor `date2` liegt:

```Rust
if date1 < date2 {
    println!("Das erste Datum liegt vor dem zweiten.");
}
```

Und wenn wir wissen wollen, ob `date1` nach `date2` liegt, können wir den `>` Operator verwenden.

Diese Operatoren können auch mit Uhrzeiten verwendet werden, um genauere Vergleiche durchzuführen. Weitere Informationen zur Arbeit mit Datum und Uhrzeit in Rust finden Sie in der Dokumentation von [chrono](https://docs.rs/chrono/0.4.19/chrono/).

## Deep Dive

Beim Vergleichen von Datumswerten müssen wir uns bewusst sein, dass es in Rust zwei verschiedene Arten gibt, um Datumswerte zu repräsentieren: `NaiveDate` und `DateTime<Utc>`. `NaiveDate` wird verwendet, wenn keine Zeitzone oder Uhrzeit angegeben ist, während `DateTime<Utc>` verwendet wird, wenn wir eine spezifische Zeitzone verwenden möchten.

Beide Typen implementieren die `PartialOrd` und `Ord` Traits, was bedeutet, dass wir die oben genannten Vergleichsoperatoren verwenden können. Wenn wir jedoch mit `DateTime<Utc>` arbeiten, müssen wir die `naive` Methode verwenden, um zuerst die Zeitzone zu entfernen und dann die Vergleichsoperation auszuführen. Ein Beispiel:

```Rust
use chrono::{DateTime, Utc, TimeZone};

let date = Utc::now();
let newer_date = Utc.ymd(2021, 6, 2).and_hms(10, 0, 0);

if date < newer_date.naive_utc() {
    println!("Das Datum ist älter als das neueste Datum.");
}
```

Indem wir die Zeitzone entfernen, können wir genauere Vergleiche durchführen, da wir sicherstellen, dass beide Werte denselben Typ haben.

## Siehe auch

- [Chrono Dokumentation](https://docs.rs/chrono/0.4.19/chrono/)
- [Rust Date and Time Guide](https://doc.rust-lang.org/std/time/)
- [Rust Cookbook: Working with Date and Time](https://rust-lang-nursery.github.io/rust-cookbook/datetime.html)