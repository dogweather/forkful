---
title:                "Arbeiten mit CSV-Dateien"
html_title:           "Arduino: Arbeiten mit CSV-Dateien"
simple_title:         "Arbeiten mit CSV-Dateien"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/working-with-csv.md"
---

{{< edit_this_page >}}

## Was & Warum?
Arbeiten mit CSV-Dateien bedeutet, Daten in einem einfachen, von Kommas getrennten Textformat zu lesen, zu schreiben und zu bearbeiten. Programmierer nutzen CSV wegen der Simplizität und der weit verbreiteten Unterstützung in Daten-Austausch und Datenspeicherung.

## Wie geht das:
```gleam
// Gleam-Modul für CSV-Möglichkeiten importieren
import gleam/csv

// CSV-Zeile parsen
fn parse_csv_line(line: String) -> Result(list(String), csv.ParseError) {
  csv.parse_line(line)
}

// CSV in String umwandeln
fn to_csv(data: list(list(String))) -> Result(String, Nil) {
  csv.from_rows(data)
}

pub fn example() {
  let line = parse_csv_line("Name,Alter,Beruf\nMax,30,Entwickler".to_string())
  assert Ok(["Name", "Alter", "Beruf"]) = line

  let data = to_csv([["Name", "Alter", "Beruf"], ["Max", "30", "Entwickler"]])
  assert Ok("Name,Alter,Beruf\nMax,30,Entwickler") = data
}
```
Sample Output:
```
OK(["Name", "Alter", "Beruf"], ["Max", "30", "Entwickler"])
```

## Vertiefung
CSV steht für "Comma-Separated Values" und wurde in den frühen 70ern populär. Heutzutage gibt es Alternativen wie JSON oder XML, die komplexere Datenstrukturen abbilden können, doch CSV bleibt relevant für einfache Tabellendaten. Implementation in Gleam erfordert meist das csv-Modul, das Parsing und Serienfunktionen bietet.

## Siehe auch
- Die offizielle Gleam-Dokumentation für CSV: https://hexdocs.pm/gleam_csv/.
- RFC 4180, der Standard für CSV-Dateiformate: https://tools.ietf.org/html/rfc4180.
- Ein Gleam CSV-Projekt auf GitHub zum reinschnuppern: https://github.com/gleam-lang/csv.