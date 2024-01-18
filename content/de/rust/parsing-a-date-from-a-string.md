---
title:                "Eine Datumsangabe aus einem String extrahieren"
html_title:           "Rust: Eine Datumsangabe aus einem String extrahieren"
simple_title:         "Eine Datumsangabe aus einem String extrahieren"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

Was & Warum?
Das Umwandeln eines Datums aus einem String ist eine gängige Aufgabe für Programmierer. Das bedeutet, dass ein Datumsausdruck in einem bestimmten Format (z.B. "12.05.2021") in ein standardisiertes Format (z.B. "2021-05-12") umgewandelt wird. Dies ist wichtig für die Verarbeitung und Speicherung von Daten in Programmen.

Wie geht's?
Das Parsen eines Datums aus einem String ist in Rust durch die Verwendung der standardmäßigen Bibliothek "chrono" einfach. Hier ist ein Beispielcode, der ein Datum aus einem String im Format "DD.MM.YYYY" umwandelt:

```Rust
use chrono::NaiveDate;

fn main() {
    let str_date = "12.05.2021";
    let date = NaiveDate::parse_from_str(str_date, "%d.%m.%Y").unwrap();
    println!("Umgeformtes Datum: {}", date);
}
```

Ausgabe: Umgeformtes Datum: 2021-05-12

Tiefer eintauchen
Die Idee des Parsens von Daten aus Strings ist nicht neu und wurde schon seit den Anfängen der Programmierung verwendet. Es ist wichtig, um sicherzustellen, dass Daten in einem standardisierten und einheitlichen Format vorliegen, um Fehler und Unklarheiten zu vermeiden. Neben der Verwendung der "chrono" Bibliothek gibt es auch andere Möglichkeiten, Daten aus Strings umzuwandeln, wie z.B. reguläre Ausdrücke oder benutzerdefinierte Funktionen.

Sieh dir auch gerne diese Quellen an:
- Dokumentation zu "chrono": https://docs.rs/chrono/0.4.19/chrono/
- "How to Parse Date and Time in Rust": https://www.educative.io/edpresso/how-to-parse-date-and-time-in-rust
- "Parsing dates and times in Rust": https://dev.to/joaquimadraz/parsing-dates-and-times-in-rust-400o