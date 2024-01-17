---
title:                "Das Berechnen eines Datums in der Zukunft oder Vergangenheit"
html_title:           "Rust: Das Berechnen eines Datums in der Zukunft oder Vergangenheit"
simple_title:         "Das Berechnen eines Datums in der Zukunft oder Vergangenheit"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Berechnen eines Datums in der Zukunft oder Vergangenheit ist ein häufiges Problem in der Programmierung. Es erfordert die Verwendung von verschiedenen Datums- und Zeitfunktionen, um das gewünschte Ergebnis zu erhalten. Programmierer benötigen diese Funktionen, um Daten aufzubereiten und zu organisieren, beispielsweise für die Planung von Ereignissen oder das Erstellen von Zeitplänen.

## Wie geht's?
Um ein Datum in der Zukunft oder Vergangenheit in Rust zu berechnen, können wir die Datetime-Bibliothek verwenden. Schauen wir uns ein Beispiel an, bei dem wir 10 Tage von einem bestimmten Datum in der Zukunft subtrahieren:

```
Rust use chrono::prelude::*;
let date = Utc.ymd(2021, 11, 20);
let ten_days_before = date - Duration::days(10);

println!("{}", ten_days_before);
```

Das obige Beispiel verwendet die Datetime-Bibliothek, um ein Datum in der Zukunft zu erstellen und dann 10 Tage davon zu subtrahieren. Die Ausgabe wird das Datum 10 Tage vor dem angegebenen Datum sein, in diesem Fall 10. November 2021.

## Tiefer Einblick
Das Berechnen von Datum und Zeit kann schwierig sein, da es viele Faktoren zu berücksichtigen gibt, wie z.B. verschiedene Zeitzonen und Schaltjahre. Alternativ können Programmierer auch die Standardbibliothek von Rust verwenden, um auf die Systemzeit zuzugreifen und damit das aktuelle Datum und die aktuelle Zeit zu erhalten.

Es gibt auch andere Bibliotheken wie DateTime und Chrono, die ähnliche Funktionen anbieten, aber die Datetime-Bibliothek ist in Rust integriert und wird von der Community unterstützt.

## Siehe auch
- [Datetime-Bibliothek in Rust](https://docs.rs/datetime/0.4.5/datetime/)
- [Chrono-Bibliothek in Rust](https://docs.rs/chrono/0.4.19/chrono/)
- [Standardbibliothek in Rust](https://doc.rust-lang.org/std/time/)