---
title:                "Berechnung eines Datums in der Zukunft oder Vergangenheit"
html_title:           "Rust: Berechnung eines Datums in der Zukunft oder Vergangenheit"
simple_title:         "Berechnung eines Datums in der Zukunft oder Vergangenheit"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Das Berechnen eines Datums in der Zukunft oder Vergangenheit kann sehr nützlich sein, zum Beispiel für die Planung von Terminen oder die Erstellung von Kalendern. Mit Rust und seinen leistungsstarken Datums- und Zeitenbibliotheken können wir dies einfach und effizient erreichen.

## Wie geht's

Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, müssen wir zuerst das aktuelle Datum bekommen und es dann um die gewünschte Anzahl an Tagen erhöhen oder reduzieren. Hier ist ein Beispiel, das das Datum von heute um 7 Tage in der Zukunft berechnet:

```Rust
use chrono::prelude::*; // Importieren der Chrono-Bibliothek

let now = Local::now(); // Aktuelles Datum bekommen
let future_date = now + Duration::days(7); // 7 Tage zu heute hinzufügen

println!("Datum in 7 Tagen: {}", future_date.format("%d.%m.%Y")); // Output: Datum in 7 Tagen: 24.07.2021
```

Und hier ist ein Beispiel, das das Datum von heute um 2 Wochen in der Vergangenheit berechnet:

```Rust
use chrono::prelude::*;

let now = Local::now();
let past_date = now - Duration::weeks(2);

println!("Datum vor 2 Wochen: {}", past_date.format("%A, %d.%m.%Y")); // Output: Datum vor 2 Wochen: Montag, 05.07.2021
```

Wie wir sehen können, können wir die Datums- und Zeitenbibliothek von Rust verwenden, um sehr präzise und formatierte Ergebnisse zu erhalten.

## Tiefer tauchen

Wenn wir genauer auf die Datums- und Zeitenbibliothek von Rust schauen, werden wir sehen, dass sie auf der ISO 8601-Standardformatierung basiert. Das bedeutet, dass das Datum in der Form "Jahr-Monat-Tag" dargestellt wird. Zum Beispiel wird der 5. Juli 2021 als "2021-07-05" angezeigt.

Außerdem können wir auch andere Funktionen der Chrono-Bibliothek nutzen, wie zum Beispiel das Berechnen von Zeitspannen oder das Konvertieren von Zeitstempeln in Datumsangaben. Die Dokumentation von Rust bietet eine detaillierte Anleitung und Beispiele zu diesen Funktionen.

## Siehe auch

- [Chrono Dokumentation](https://docs.rs/chrono)
- [How to Use DateTime, Calendar and Date Classes in Rust](https://www.section.io/engineering-education/how-to-use-datetime-calendar-and-date-classes-in-rust/)
- [Creating Dates and Time in Rust using chrono](https://www.section.io/engineering-education/creating-date-and-time-in-rust-using-chrono/)