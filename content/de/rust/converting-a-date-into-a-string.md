---
title:                "Ein Datum in einen String umwandeln"
html_title:           "Java: Ein Datum in einen String umwandeln"
simple_title:         "Ein Datum in einen String umwandeln"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Daten in einen String umzuwandeln bedeutet, eine Darstellung des Datums als Text zu erstellen, die von Menschen leicht gelesen werden kann. Programmierer tun dies, um Daten in eine nützlichere und handhabbare Textform zu bringen, die in Benutzeroberflächen, Protokollen, Berichten usw. angezeigt werden kann.


## So geht's:

In Rust können wir das Chrono-Crate verwenden, um mit Daten zu arbeiten und sie in Zeichenketten zu konvertieren. Fügen Sie `chrono = "0.4.19"` zu Ihren `Cargo.toml`-Abhängigkeiten hinzu.

```Rust
extern crate chrono;
use chrono::{Utc, DateTime, NaiveDate, NaiveTime};

fn main() {
    let now: DateTime<Utc> = Utc::now();
    println!("Jetzt: {}", now);
    let datum: NaiveDate = NaiveDate::from_ymd(2020, 12, 25);
    let zeit: NaiveTime = NaiveTime::from_hms(6, 30, 0);
    println!("Datum & Zeit: {} {}", datum, zeit);
}
```

Probiere diesen Code aus und du wirst etwas ähnliches wie das Folgende sehen:

```Rust
Jetzt: 2022-03-17 14:31:45.690908200 UTC
Datum & Zeit: 2020-12-25 06:30:00
```


## Vertiefung:

* **Historischer Kontext:** Rust wurde entwickelt, um Speichersicherheit zu gewährleisten, ohne die Geschwindigkeit zu beeinträchtigen. Bei der Arbeit mit Daten kann man sich nicht auf die eingebauten Funktionen verlassen und benötigt Pakete wie das Chrono-Crate.
* **Alternativen:** Es gibt andere Crates wie `time` oder `date-fmt`, aber `chrono` ist die verbreitetste und gut dokumentierte Daten- und Zeitbibliothek in Rust.
* **Implementierungsdetails:** `chrono` verfügt über eine Vielzahl von Funktionen, um Daten zu manipulieren und in Zeichenketten zu konvertieren. Zum Beispiel der DateTime-Typ, der eine Kombination aus Datum und Zeit darstellt.


## Siehe auch:

Für weitere Informationen besuchen Sie:

* [Rust strftime Dokumentation](https://docs.rs/chrono/0.4.19/chrono/format/strftime/index.html)
* [Rust Chrono Repository auf GitHub](https://github.com/chronotope/chrono)
* [Rust function/method documentation](https://doc.rust-lang.org/std/)