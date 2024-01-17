---
title:                "Arbeiten mit CSV"
html_title:           "Rust: Arbeiten mit CSV"
simple_title:         "Arbeiten mit CSV"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/working-with-csv.md"
---

{{< edit_this_page >}}

## Was ist CSV und warum nutzen Programmierer es?

CSV steht für "Comma Separated Values" und ist ein verbreitetes Dateiformat, das zur Speicherung von tabellarischen Daten verwendet wird. Programmierer nutzen oft CSV-Dateien, weil sie einfach zu erstellen und zu lesen sind und eine gute Möglichkeit bieten, strukturierte Daten zu speichern.

## Anleitung:

Um mit CSV-Dateien in Rust zu arbeiten, benötigen wir das Paket "csv". Hier ist ein Beispiel, wie wir eine CSV-Datei einlesen und die Daten in eine Vektorsammlung speichern können:

```Rust
use csv;
use std::error::Error;
use std::path::Path;

fn main() -> Result<(), Box<dyn Error>> {
    let path = Path::new("data.csv");
    let mut reader = csv::Reader::from_path(path)?;
    let mut data: Vec<Vec<String>> = Vec::new();
    for result in reader.records() {
        let record = result?;
        let row: Vec<String> = record.iter().map(|field| field.to_string()).collect();
        data.push(row);
    }
    println!("{:?}", data);
    Ok(())
}
```

Die Ausgabe könnte wie folgt aussehen:

```
[["Name", "Alter", "Beruf"], ["Max", "24", "Programmierer"], ["Anna", "32", "Designer"], ["Tom", "28", "Student"]]
```

## Tiefergehende Informationen:

CSV wurde in den 1970er Jahren entwickelt und ist seitdem zu einem der am häufigsten verwendeten Dateiformate geworden. Es gibt auch alternative Formate wie TSV (Tab Separated Values), bei denen Tabulatoren statt Kommas als Trennzeichen verwendet werden.

Das "csv" Paket in Rust bietet eine Vielzahl von Funktionen, die es uns ermöglichen, CSV-Dateien effizient zu verarbeiten. Wir können auch Optionen wie das Trennzeichen, die Zeilenbegrenzung und das Encoding anpassen.

## Weitere Ressourcen:

- Offizielle Dokumentation des "csv" Pakets in Rust: https://docs.rs/csv/
- Beispielcode für das Schreiben von Daten in eine CSV-Datei: https://gist.github.com/weiznich/3642f6e787a7f0302ed5
- Ein Einführungsvideo in die Verwendung von CSV in Rust: https://www.youtube.com/watch?v=OXVpwpn0-K4