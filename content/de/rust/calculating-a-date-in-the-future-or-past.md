---
title:    "Rust: Berechnung eines Datums in der Zukunft oder Vergangenheit"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

In der Welt der Programmierung gibt es immer wieder die Herausforderung, ein Datum in der Vergangenheit oder Zukunft zu berechnen. Egal ob es sich um Geburtstage, Fristen oder Feiertage handelt, es ist wichtig, dass wir als Entwickler*innen die korrekten Daten berechnen können. In diesem Blogbeitrag werden wir uns anschauen, wie man dies in der Programmiersprache Rust lösen kann.

## How To

Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, müssen wir zuerst das aktuelle Datum als Basis nehmen. In Rust können wir dies mit der Standardbibliothek "chrono" erreichen. Hier ist ein Beispielcode, in dem wir das aktuelle Datum berechnen und danach das Datum des nächsten Tages ausgeben:

```rust
use chrono::{Utc, Datelike};

fn main() {
    let current_date = Utc::today();
    let next_day = current_date + chrono::Duration::days(1);
    println!("Das aktuelle Datum ist {}. Der nächste Tag ist {}.", current_date, next_day);
}
```

Die Ausgabe dieses Codes wird folgendermaßen aussehen:

`Das aktuelle Datum ist 2021-11-10. Der nächste Tag ist 2021-11-11.`

In diesem Beispiel haben wir die `today()` Methode verwendet, um das aktuelle Datum zu erhalten. Danach haben wir die `Duration::days()` Methode verwendet, um eine Dauer von einem Tag zu erstellen und diese zur aktuellen Datum hinzuzufügen. Natürlich können wir diese Dauer auch anpassen, um ein Datum in der Zukunft oder Vergangenheit zu berechnen.

## Deep Dive

Die "chrono" Bibliothek bietet uns viele weitere Funktionen und Möglichkeiten, um mit Datum und Uhrzeit in Rust umzugehen. Hier sind einige interessante Funktionen, die uns bei der Berechnung von Datums- und Zeiten helfen könnten:

- Die `Date` Struktur ermöglicht es uns, individuelle Komponenten eines Datums (Tag, Monat, Jahr) auszulesen und zu bearbeiten.
- Die `DateTime` Struktur bietet ähnliche Funktionen wie die `Date` Struktur, aber auch die Möglichkeit, die Uhrzeit einzuschließen.
- Die `Offset` Struktur ermöglicht es uns, einen Zeitversatz hinzuzufügen oder zu subtrahieren, um verschiedene Zeitzonen zu berücksichtigen.
- Die `weekday()` Methode gibt uns den Wochentag des Datums zurück.
- Die `format()` Methode ermöglicht es uns, ein Datum in verschiedenen Formaten auszugeben.

Es lohnt sich auf jeden Fall, sich mit der "chrono" Bibliothek auseinanderzusetzen und ihre vielfältigen Funktionen zu erkunden, um vollständigere und genaue Datumsberechnungen durchzuführen.

## Siehe auch

- [Offizielle "chrono" Dokumentation](https://docs.rs/chrono/0.4.19/chrono/)
- [Weitere Beispiele und Tutorials zu "chrono"](https://rust-lang-nursery.github.io/rust-cookbook/datetime.html)