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

## Warum

CSV (Comma Separated Values) ist eines der gängigsten Dateiformate für den Austausch von Daten. Es ermöglicht eine einfache und strukturierte Darstellung von Daten, die von fast allen Anwendungen gelesen und verarbeitet werden können. In diesem Artikel werden wir sehen, wie Gleam uns dabei helfen kann, effizient mit CSV-Dateien zu arbeiten.

## Wie geht das?

Zunächst müssen wir das Gleam-Modul `csv` importieren, um auf die Funktionen zuzugreifen, die uns bei der Arbeit mit CSV helfen werden. Dann können wir eine CSV-Datei öffnen und die Daten in eine Variable speichern. Die Methode `csv.from_string` nimmt den Inhalt der Datei als Eingabe und liefert eine Liste von Listen zurück, wobei jede innere Liste eine Zeile der CSV-Datei darstellt. Hier ist ein Beispielcode, um die Daten aus einer CSV-Datei namens `daten.csv` zu lesen:

```Gleam
let daten = import("csv").from_string(
    let csv = "vorname,nachname,alter\n
               Max,Mustermann,32\n
               Anna,Müller,28\n"
in csv
)
```

Wir können auch ähnlich mit der Methode `csv.from_file` arbeiten, die statt des Dateiinhalts den Dateinamen als Eingabe erwartet. Mit der Methode `csv.to_string` können wir auch Daten im CSV-Format aus einer Liste von Listen erstellen. Hier ist ein Beispiel, um die Daten aus der vorherigen Variable `daten` in das CSV-Format zu konvertieren und in einer Datei namens `neue_daten.csv` zu speichern:

```Gleam
import("csv").to_string(daten)
          |> File.write("neue_daten.csv")
```

Das Gleam-Modul bietet auch Funktionen, um Daten in einem bestimmten Format auszugeben. Hier ist ein Beispiel, das die Daten aus `daten` im JSON-Format ausgibt:

```Gleam
import("json").encode(import("csv").to_record(daten))
```

Es ist auch möglich, Spaltennamen bei der Verwendung von Gleam mit CSV-Dateien zu berücksichtigen. Dabei müssen wir das Modul `csv` mit dem zusätzlichen Argument `headers: true` importieren. Dann müssen wir die CSV-Datei mit der Methode `csv.from_string` oder `csv.from_file` einlesen, wobei wir das Argument `headers: ["vorname", "nachname", "alter"]` angeben müssen, um die Spaltennamen anzugeben. Hier ist ein Beispiel, das Daten aus einer CSV-Datei mit Spaltennamen liest:

```Gleam
let daten = import("csv", headers: true).from_string(
               let csv = "vorname,nachname,alter\n
                          Max,Mustermann,32\n
                          Anna,Müller,28\n"
               in csv,
               headers: ["vorname", "nachname", "alter"])  
```

## Tiefer Einblick

Das Gleam-Modul `csv` bietet viele weitere Funktionen, die es uns ermöglichen, Daten in verschiedenen Formaten auszugeben oder zu konvertieren. Es ist auch möglich, benutzerdefinierte CSV-Separatoren oder Zeilenumbrüche zu verwenden. Für weitere Details und Beispiele, siehe die offizielle Dokumentation des Moduls.

## Siehe auch

- [Offizielle Dokumentation des csv-Moduls](https://gleam.run/modules/csv)
- [CSV-Wikipedia-Artikel (auf Deutsch)](https://de.wikipedia.org/wiki/CSV_(Dateiformat))