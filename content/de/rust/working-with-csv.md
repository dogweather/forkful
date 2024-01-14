---
title:                "Rust: Arbeiten mit CSV"
simple_title:         "Arbeiten mit CSV"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/working-with-csv.md"
---

{{< edit_this_page >}}

## Warum

CSV (Comma Separated Values) ist ein gängiges Datenformat, das häufig von Unternehmen und Organisationen verwendet wird, um Tabellen- und Listenformatdaten zu speichern. Als Programmierer könnte es nützlich sein, mit CSV-Dateien zu arbeiten, um beispielsweise Daten für Analysen oder Visualisierungen zu verarbeiten. In diesem Blog-Beitrag werden wir uns mit der Arbeit mit CSV-Dateien in der Programmiersprache Rust befassen.

## Wie man

Um mit CSV-Dateien in Rust zu arbeiten, benötigen wir zunächst das Paket `csv` aus der Rust Standardbibliothek und das Paket `serde` aus der Crate-Community. Mit `serde` können wir CSV-Daten in benutzerdefinierte Rust-Datentypen serialisieren und deserialisieren. Schauen wir uns ein Beispiel an:

```Rust
// Importieren der Pakete
extern crate csv;
extern crate serde;
extern crate serde_derive;

// Struct zum Festlegen der Spalten im CSV
#[derive(Debug, Deserialize, Serialize)]
struct Person {
    name: String,
    age: u8,
    city: String,
}

fn main() {
    // Öffnen der CSV-Datei
    let mut reader = csv::Reader::from_path("personen.csv").unwrap();

    // Iterieren über jede Zeile in der CSV
    for result in reader.deserialize() {
        // Ergebnis in ein `Person`-Objekt konvertieren
        let person: Person = result.unwrap();
        // Zugriff auf die Daten der Person
        println!("Hallo {} aus {}!", person.name, person.city);
    }
}
```

Die CSV-Datei `personen.csv` muss die folgenden Spalten enthalten (ohne Überschriftenzeile):

```
Max,35,Berlin
Anna,28,Hamburg
Peter,42,München
```

Die Ausgabe unseres Codes wird folgendermaßen aussehen:

```
Hallo Max aus Berlin!
Hallo Anna aus Hamburg!
Hallo Peter aus München!
```

## Tief eintauchen

In dem obigen Beispiel haben wir eine einfache `Person`-Struktur definiert und diese mit Daten aus einer CSV-Datei gefüllt. Aber was ist, wenn die Spalten der CSV-Datei nicht mit unseren Strukturen übereinstimmen? In diesem Fall können wir benutzerdefinierte Deserialisierer verwenden, um CSV-Daten in beliebige Rust-Datentypen zu konvertieren.

Ein weiteres nützliches Feature von `serde` ist die Möglichkeit, Serailisierungs- und Deserialisierungsfehler abzufangen und zu verarbeiten. Dies ist besonders wichtig, wenn wir mit großen CSV-Dateien arbeiten, bei denen Fehler auftreten können.

## Siehe auch

- [Rust Standardbibliothek Dokumentation für CSV](https://doc.rust-lang.org/std/io/trait.Read.html)
- [Serde-Dokumentation für CSV](https://docs.rs/serde_derive/1.0.104/serde_derive/)
- [Rust Community Crates für CSV](https://crates.io/search?q=csv)