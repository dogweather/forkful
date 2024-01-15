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

## Warum

CSV (Comma Separated Values) ist ein weit verbreitetes Datenformat für den Austausch von Tabellen und einfach strukturierten Daten. Da Rust eine Sprache ist, die auf Performance, Sicherheit und Parallelität ausgelegt ist, kann es eine gute Wahl sein, CSV-Dateien zu analysieren oder zu generieren.

## Wie

### Lesen von CSV-Dateien

Um eine CSV-Datei zu lesen, können Sie die `csv`-Bibliothek verwenden. Zunächst müssen Sie diese in Ihrem Projekt importieren:

```Rust
extern crate csv;

use std::error::Error;
use std::fs::File;

use csv::ReaderBuilder;
```

Als nächstes können Sie die Datei mit dem `ReaderBuilder` öffnen und die Daten in einem Vektor speichern:

```Rust
let file = File::open("daten.csv")?;
let mut reader = ReaderBuilder::new().from_reader(file);

// Vektor zum Speichern der Daten initialisieren
let mut daten: Vec<Record> = vec![];

// Daten iterativ in den Vektor speichern
for result in &mut reader.records() {
    let record = result?;
    daten.push(record);
}
```

Das `Record`-Objekt enthält die Daten jeder Zeile der CSV-Datei. Sie können nun auf die Daten zugreifen und diese weiterverarbeiten.

### Schreiben von CSV-Dateien

Wenn Sie Daten in eine CSV-Datei schreiben möchten, können Sie dies mit der `Writer`-Klasse tun:

```Rust
let file = File::create("ergebnisse.csv")?;
let mut writer = csv::Writer::from_writer(file);

// Daten aus einem Vektor schreiben
writer.write_record(&["Name", "Alter", "Stadt"])?;
writer.write_record(&["Max", "25", "Berlin"])?;
writer.write_record(&["Lisa", "31", "Hamburg"])?;
```

Im obigen Beispiel werden drei Zeilen mit den entsprechenden Spalten geschrieben. Beachten Sie, dass Sie die Daten als Referenz übergeben müssen, da die `Writer`-Klasse eine `&[&str]`-Argument erwartet.

## Deep Dive

Wenn Sie tiefer in die Arbeit mit CSV-Dateien einsteigen möchten, können Sie sich mit der Dokumentation der `csv`-Bibliothek vertraut machen. Außerdem gibt es weitere Bibliotheken wie zum Beispiel `serde_csv`, die eine vereinfachte Schnittstelle für das Lesen und Schreiben von CSV-Dateien bieten.

Eine wichtige Sache, die Sie beachten sollten, ist die Formatierung von CSV-Dateien. Während die meisten Programme Kommas als Trennzeichen verwenden, gibt es auch andere Möglichkeiten wie zum Beispiel Semikolons oder Tabs. Außerdem müssen Sonderzeichen wie Anführungszeichen oder Zeilenumbrüche richtig behandelt werden, um Fehler bei der Verarbeitung der Daten zu vermeiden.

## Siehe auch

- [Dokumentation der CSV-Bibliothek](https://github.com/BurntSushi/rust-csv)
- [Serde-CSV Bibliothek](https://github.com/BurntSushi/rust-csv)
- [Rust Programmiertutorial](https://www.rust-lang.org/learn)