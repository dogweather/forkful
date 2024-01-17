---
title:                "Vergleich zweier Daten"
html_title:           "Rust: Vergleich zweier Daten"
simple_title:         "Vergleich zweier Daten"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Vergleichen von zwei Daten in Rust

## Was & Warum?
Das Vergleichen von zwei Daten ist eine übliche Aufgabe in der Programmierung, bei der man feststellt, welches Datum früher oder später ist. Dies ist besonders hilfreich, wenn man Daten verarbeiten oder sortieren möchte. Programmierer führen diese Aufgabe häufig aus, um die Logik von Programmen zu steuern und um Fehlfunktionen zu vermeiden.

## Wie geht's:
In Rust kann man zwei Daten mit den Vergleichsoperatoren `==`, `!=`, `<`, `>`, `<=` und `>=` vergleichen. Diese operatoren werden mit zwei Daten als Operanden verwendet und geben einen `bool` (true oder false) zurück.

```Rust
let date1 = NaiveDate::from_ymd(2020, 4, 15);
let date2 = NaiveDate::from_ymd(2020, 4, 20);
println!("{}", date1 < date2); // true
println!("{}", date1 == date2); // false
```

## Tiefere Einblicke:
Die Vergleichsoperation ist in der Programmierung schon lange bekannt und wird in den meisten Sprachen ähnlich implementiert. In Rust gibt es jedoch die Besonderheit, dass man auch komplexe Datenstrukturen wie Datumsobjekte vergleichen kann, solange diese die erforderlichen Implementierungen für die Vergleichsoperatoren haben.

Alternativen zum Vergleichen von Daten könnten zum Beispiel die Verwendung von Timestamps oder komplett andere Ansätze wie die Umwandlung von Daten in Zahlenwerte sein. Es kommt immer auf den Anwendungsfall an.

Die genaue Implementierung der Vergleichsoperatoren in Rust ist in der Rust-Dokumentation erklärt: https://doc.rust-lang.org/std/cmp/trait.PartialEq.html

## Siehe auch:
- Rust-Dokumentation: https://www.rust-lang.org/
- Mehr über die Datei- und Zeitverarbeitung in Rust: https://docs.rs/chrono/0.4.0/chrono/
- Eine umfassende Einführung in Rust: https://doc.rust-lang.org/book/