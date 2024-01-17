---
title:                "Arbeiten mit CSV"
html_title:           "Gleam: Arbeiten mit CSV"
simple_title:         "Arbeiten mit CSV"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/working-with-csv.md"
---

{{< edit_this_page >}}

## Was ist CSV und warum verwenden Programmierer es?
CSV steht für "Comma-Separated Values" und ist ein Dateiformat, das verwendet wird, um Daten in tabellarischer Form zu speichern, ähnlich wie eine Tabelle in einer Datenbank. Programmierer verwenden CSV, um Daten zu organisieren, auszutauschen und zu analysieren, da es plattformunabhängig ist und von vielen Programmen, einschließlich Gleam, unterstützt wird.

## So geht's:
Um CSV-Dateien in Gleam zu nutzen, müssen Sie zuerst das Paket "csv" Ihrem Projekt hinzufügen, indem Sie `pubmod { csv }` in Ihre `gleam.toml` Datei eingeben. Dann können Sie Funktionen wie `read` und `write` verwenden, um auf CSV-Dateien zuzugreifen und sie zu bearbeiten.

```Gleam
// Lesen einer CSV-Datei
import csv

let file = csv.open("meine_datei.csv") // Datei öffnen
let daten = file.read() // Datei-Inhalt lesen
match daten {
  Ok(daten) -> daten  // Mit den Daten arbeiten
  Err(e) -> panic(e)  // Fehlerbehandlung
}

// Schreiben in eine CSV-Datei
let daten = [["Name", "Alter"], ["Max", "30"], ["Lisa", "25"]] // Daten vorbereiten
let file = csv.create("meine_neue_datei.csv") // Neue Datei erstellen
file.write(daten) // Daten in die Datei schreiben

// Ausgabe:
// Name,Alter
// Max,30
// Lisa,25
```

## Tiefere Einblicke:
CSV wurde ursprünglich in den 1970er Jahren entwickelt und hat seitdem an Bedeutung gewonnen, da es einfach zu lesen und zu schreiben ist und von den meisten Tabellenkalkulationsprogrammen unterstützt wird. Es gibt auch alternative Dateiformate wie XML oder JSON, die ebenfalls von Programmierern genutzt werden können. Die Gleam-Implementierung für CSV basiert auf der Jiffy-CSV-Bibliothek und unterstützt auch das Lesen von CSV-Dateien mit Optionen für benutzerdefinierte CSV-Parser.

## Siehe auch:
- Dokumentation des Gleam-CSV-Pakets: https://github.com/gleam-lang/csv
- Jiffy-CSV-Bibliothek: https://jiffyclub.github.io/csv/
- Ein Vergleich von CSV mit anderen Dateiformaten: https://www.opencsv.org/articles.html