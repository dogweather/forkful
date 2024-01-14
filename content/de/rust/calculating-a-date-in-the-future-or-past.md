---
title:                "Rust: Berechnung eines Datums in der Zukunft oder Vergangenheit"
programming_language: "Rust"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum 

Das Berechnen eines Datums in der Zukunft oder Vergangenheit kann für viele Anwendungsfälle nützlich sein, wie z.B. die Planung von Aufgaben oder die Erstellung von Zeitplänen. In diesem Blog-Beitrag lernst du, wie du dies mit Rust programmieren kannst.

## Wie geht man vor

Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, müssen wir zunächst das [`chrono` Paket](https://docs.rs/chrono/latest/chrono/) installieren und in unserem Code importieren. Dann können wir die Funktion `days` verwenden, um eine bestimmte Anzahl von Tagen zum aktuellen Datum hinzuzufügen oder davon abzuziehen.

```Rust
use chrono::{Duration, Local};

// Berechne das Datum, welches 20 Tage in der Zukunft liegt
let future_date = Local::today() + Duration::days(20);

// Berechne das Datum, welches 10 Tage in der Vergangenheit liegt
let past_date = Local::today() - Duration::days(10);

// Gib das Ergebnis aus
println!("Zukünftiges Datum: {}", future_date);
println!("Vergangenes Datum: {}", past_date);
```

Dieses Code-Beispiel verwendet die aktuelle lokale Zeit, aber du kannst auch eine spezifische Zeitzone angeben, indem du die `FixedOffset` Funktion benutzt.

## Tiefgehende Analyse

Das `chrono` Paket bietet auch viele weitere Funktionen, um mit Datums- und Zeitberechnungen zu arbeiten. Du kannst z.B. bestimmte Wochentage oder Monate berechnen, die Anzahl der vergangenen Tage zwischen zwei Daten bestimmen oder Zeitintervalle hinzufügen.

Es ist auch möglich, Datumseingaben von Benutzern über die `parse_from_str` Funktion zu verarbeiten und sie dann in ein bestimmtes Datumsformat zu konvertieren.

## Siehe auch

- [Chrono Dokumentation](https://docs.rs/chrono/latest/chrono/)
- [Date Calculation in Rust](https://medium.com/@phoomparin/calculating-date-in-rust-166c3b811def)