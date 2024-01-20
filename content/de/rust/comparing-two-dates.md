---
title:                "Vergleich von zwei Daten"
html_title:           "C#: Vergleich von zwei Daten"
simple_title:         "Vergleich von zwei Daten"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Was & Warum?
Datumsvergleich bedeutet, zwei verschiedene Datenpunkte (Datum und Uhrzeit) auf ihre zeitliche Beziehung zueinander zu prüfen - ist Datum A vor, nach oder gleich wie Datum B? Dies ist nützlich in Situationen, in denen wir zeitabhängige Logik programmieren, wie z.B. To-Do-Listen oder Terminplanung.

## So geht's:
In Rust könnten wir das chrono crate verwenden, um zwei Daten zu vergleichen. Hier ist ein einfacher Code.
```Rust
extern crate chrono;
use chrono::{DateTime, Utc};

fn main() {
    let datum_a: DateTime<Utc> = Utc::now();
    let datum_b: DateTime<Utc> = Utc::now() + chrono::Duration::seconds(10);

    if datum_a < datum_b {
        println!("Datum A ist früher als Datum B");
    } else if datum_a > datum_b {
        println!("Datum A ist später als Datum B");
    } else {
        println!("Datum A und Datum B sind gleich");
    }
}
```
In diesem Fall zeigt die Ausgabe an, dass Datum A früher als Datum B ist.

## Vertiefung
Historisch gesehen, konnte das Vergleichen von zwei Daten in Rust schwierig sein, da die Standardbibliothek Rusts zuerst keine Methoden zur Handhabung von Zeit und Datum bereitgestellt hat. Dies führte zur Entwicklung von crates wie chrono, die diese Funktionalitäten bereitstellen. 

Alternativen zum Vergleichen von Daten in Rust könnten das Aufbrechen des Datums in Teile, wie Jahr, Monat und Tag und diese Elemente einzeln zu vergleichen beinhalten, jedoch ist dies umständlicher und anfälliger für Fehler.

Beim Vergleichen von Daten ist zu beachten, dass Zeitzonen massiven Einfluss auf die Resultate haben können. Beispielsweise kann ein Datum und Uhrzeit in Berlin unterschiedlich zu demselben Datum und Uhrzeit in New York sein.

## Siehe auch
* Die [chrono crate Dokumentation](https://docs.rs/chrono/0.4.19/chrono/) bietet tiefschürfende Informationen und Beispiele.
* Für eine gründliche Einführung in die Handhabung von Datum und Zeit in Rust, siehe [dieser Artikel](https://programmingzen.com/handling-date-and-time-in-rust/) von Antonio Cangiano.
* Eine Diskussion über das Vergleichen von Daten in Rust kann [im Rust-Forum](https://users.rust-lang.org/t/comparing-dates-in-rust/2032) gefunden werden.