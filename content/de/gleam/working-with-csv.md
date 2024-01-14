---
title:                "Gleam: Arbeiten mit csv"
simple_title:         "Arbeiten mit csv"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/working-with-csv.md"
---

{{< edit_this_page >}}

## Warum

CSV (Comma-Separated Values) ist ein häufig verwendetes Dateiformat für den Austausch von Daten. Es ist einfach zu lesen und zu schreiben, was es zu einer beliebten Wahl für die Verarbeitung von Daten in Programmen macht. In Gleam können wir CSVs mithilfe der `gleam-csv` Bibliothek leicht verarbeiten. In diesem Blogbeitrag werden wir sehen, wie wir CSVs in Gleam einlesen, verarbeiten und schreiben können.

## Wie geht das

Um mit CSV in Gleam zu arbeiten, müssen wir zunächst die `gleam-csv` Bibliothek in unserem Projekt verwenden. Dazu fügen wir sie einfach zur `deps` Sektion unserer `gleam.toml` Datei hinzu:

```toml
[deps]
gleam-csv = "0.1.0"
```

Als nächstes importieren wir die Bibliothek in unserem Code:

```gleam
import csv
```

### CSV einlesen

Um eine CSV-Datei einzulesen, verwenden wir die Funktion `csv.from_file`. Diese Funktion erwartet zwei Argumente: den Dateipfad zur CSV-Datei und den Namen der Spalten. Die Spaltennamen sind optional und können als Liste von Strings übergeben werden, oder wir können sie aus der ersten Zeile der CSV-Datei automatisch extrahieren lassen.

```gleam
csv.from_file("my_file.csv", ["Name", "Alter", "Stadt"])
|> Ok(map(csv => io.println(csv)))
```

Das Ergebnis wird als `Result` Typ zurückgegeben, da das Einlesen der Datei fehlschlagen könnte.

### CSV verarbeiten

Wir können nun die Funktionen der `gleam-csv` Bibliothek verwenden, um unsere CSV-Daten zu verarbeiten. Hier sind einige Beispieloperationen:

- `csv.get` für den Zugriff auf Daten einer bestimmten Zeile und Spalte
- `csv.map` für die Anwendung einer Funktion auf jede Zeile der CSV-Datei
- `csv.filter` für die Filterung von Zeilen basierend auf einem bestimmten Kriterium

```gleam
csv.from_file("my_file.csv")
|> Ok(map(csv => csv.map(x => x."Alter" |> String.toInt())))
```

### CSV schreiben

Um CSV-Dateien in Gleam zu schreiben, verwenden wir die Funktion `csv.to_string`. Diese Funktion erwartet zwei Argumente: die Daten, die in CSV-Format umgewandelt werden sollen, und die Spaltennamen (optional). Das Ergebnis ist ein String, den wir dann in eine Datei schreiben können.

```gleam
let data =
  [{Name="Max", Alter=24, Stadt="München"}, {Name="Lisa", Alter=30, Stadt="Berlin"}]

let columns = ["Name", "Alter", "Stadt"]

csv.to_string(data, columns)
|> io.write_file("output.csv")
```

## Tiefgehende Analyse

Beim Umgang mit CSV in Gleam müssen wir darauf achten, dass die Spaltennamen in unserer Datenstruktur mit denen in der CSV-Datei übereinstimmen. Andernfalls können wir die `csv.get` Funktion nicht verwenden, um auf bestimmte Spalten zuzugreifen. Außerdem müssen wir sicherstellen, dass wir Daten im richtigen Datentyp aus der CSV-Datei auslesen oder sie entsprechend konvertieren.

## Siehe auch

- Dokumentation der `gleam-csv` Bibliothek - https://github.com/onyxite/gleam-csv
- Gleam-Tutorial zur Arbeit mit CSV - https://gleam.run/tutorials/csv