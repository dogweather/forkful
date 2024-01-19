---
title:                "Einen Datum aus einem String parsen"
html_title:           "Elixir: Einen Datum aus einem String parsen"
simple_title:         "Einen Datum aus einem String parsen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Parsen eines Datums aus einem String ist der Prozess, bei dem ein Textdatumsformat in ein datumsfähiges Format konvertiert wird. Wir tun dies, um Datumsdaten besser zu manipulieren und zu vergleichen.

## So funktioniert's:

In Rust kann man mithilfe der 'Chrono'-Bibliothek ein Datum parsen. Hier ist ein einfaches Beispiel:

```Rust 
extern crate chrono;
use chrono::NaiveDate;

fn main() {
    let dt = NaiveDate::parse_from_str("2022-10-12", "%Y-%m-%d").unwrap();
    println!("{}", dt);  
}
```

Das obige Programm gibt `2022-10-12` aus.

## Deep Dive

Das Parsen von Datumswerten aus Strings ist eine lang geübte Praxis in der Programmierung. Es hat seine Wurzeln in der Notwendigkeit, Daten in menschenlesbaren Textformaten zu speichern und zu übertragen, die anschließend leicht in maschinenverarbeitbare Formate konvertiert werden können.

Es gibt Alternative Lösungen, wie using `time`-Bibliothek oder direkt mit `std::time`-Bibliothek arbeiten. Aber `Chrono` ist einfach zu verwenden und bietet auch Zeitzonen-Unterstützung, was bei den anderen beiden fehlt.

`Chrono` wird verwendet, weil es Funktionen bietet, die das menschenlesbare Format (String) eines Datums in eine für die Maschine verständliche Form parsen können.

## Siehe auch 

Hier sind einige zusätzliche Ressourcen zu diesem Thema:

1. Die offizielle Dokumentation zur `Chrono`-Bibliothek: https://docs.rs/chrono/0.4.19/chrono/
2. Ein Tutorial zur Verwendung der `Chrono`-Bibliothek in Rust: https://www.forrestthewoods.com/blog/how-to-display-localized-dates-in-rust/
3. Rust-Datums- und Zeitdokumentation: https://stevedonovan.github.io/rustifications/2018/09/08/common-rust-lifetime-misconceptions.html