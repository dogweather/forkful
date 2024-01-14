---
title:                "Rust: Datumsberechnung in der Zukunft oder Vergangenheit"
simple_title:         "Datumsberechnung in der Zukunft oder Vergangenheit"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Warum

Das Berechnen von Datumsangaben in der Zukunft oder Vergangenheit kann in verschiedenen Situationen nützlich sein, beispielsweise beim Erstellen von Kalendern oder bei der Planung von Terminen. In diesem Blogpost erfährst du, wie du mit Rust solche Berechnungen durchführen kannst.

# Wie man es macht

Zunächst müssen wir das `chrono`-Paket in unser Projekt einbinden, um die benötigten Funktionen zur Datumsberechnung nutzen zu können.

```Rust
use chrono::{DateTime, NaiveDate, NaiveDateTime, NaiveTime, Utc};
```

Um ein bestimmtes Datum zu berechnen, benötigen wir ein Ausgangsdatum und die Anzahl der gewünschten Tage, Monate oder Jahre, die wir hinzufügen oder subtrahieren möchten.

```Rust
let date = NaiveDate::from_ymd(2021, 10, 10);
let date_in_past = date - Duration::days(10);
let date_in_future = date + Duration::years(2);
```

Alternativ können wir auch direkt ein aktuelles Datum erstellen und berechnen.

```Rust
let now = Utc::now();
let future_date = now + Duration::days(30);
```

Um einen Zeitstempel zu berechnen, können wir auch die `DateTime`-Struktur nutzen, die Datum und Uhrzeit kombiniert.

```Rust
let datetime = NaiveDateTime::new(date, time);
let new_datetime = datetime
    .checked_add_signed(Duration::hours(5))
    .expect("Valid timestamp"); //um einen Überlauf zu verhindern
```

# Tiefer tauchen

Beim Berechnen von Datumsangaben in die Zukunft oder Vergangenheit müssen wir berücksichtigen, dass es unterschiedliche Zeitzonen und Kalendersysteme gibt. Mit der `DateTime`-Struktur können wir Datum und Uhrzeit in der UTC-Zeitzone speichern und anzeigen. Auch die Anpassung an verschiedene Kalendersysteme ist möglich, indem wir die entsprechenden Funktionen aus dem `chrono`-Paket nutzen.

Es gibt auch weitere Funktionen zum Berechnen von Datumsangaben, wie zum Beispiel das Vergleichen von zwei Daten oder das Konvertieren von einem Zeichenfolgenformat in ein Datum.

# Siehe auch

- [Rust-Dokumentation für das `chrono`-Paket](https://docs.rs/chrono/0.4.19/chrono/)
- [Rust-Kurs: Datumsangaben berechnen](https://www.youtube.com/watch?v=mJrw8q9Xelg)
- [Calendar Calculator für Rust](https://crates.io/crates/calendar)