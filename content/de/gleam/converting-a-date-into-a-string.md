---
title:                "Ein Datum in einen String umwandeln"
html_title:           "Java: Ein Datum in einen String umwandeln"
simple_title:         "Ein Datum in einen String umwandeln"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Datum in String konvertieren: Eine Anleitung für das Gleam-Programmieren

## Was & Warum?

Das Konvertieren eines Datums in eine Zeichenkette (String) ist eine grundlegende Operation in der Programmierung, die es ermöglicht, Daten in lesbarer Form zu präsentieren oder zu speichern. Programmierer machen das oft, um eine bessere, menschenlesbare Darstellung von Datumsinformationen zu ermöglichen.

## Wie geht das?

In Gleam kann ein Datum mit dem `to_string` Funktion des `calendar` Moduls in eine Zeichenkette umgewandelt werden. Hier ist ein einfaches Beispiel:

```gleam
import gleam/calendar

let geburtstag = calendar.new(1990, 12, 28)

let geburtstag_string = calendar.to_string(geburtstag)
```

Dies gibt die Zeichenkette `"1990-12-28"` zurück.

## Vertiefung

Die Funktion `to_string` in Gleam wurde entwickelt, um die Umwandlung von Datumsobjekten in Zeichenketten zu erleichtern und folgt der Tradition der meisten modernen Hochsprachen. Vor der Einführung von Funktionen wie `to_string` mussten Programmierer oft mehrere Schritten durchlaufen, um ein Datum in eine formatierte Zeichenkette zu konvertieren.

Es gibt verschiedene Methoden, um ein Datum in einen String zu konvertieren. In Gleam könnte man zum Beispiel auch die `format` Funktion des `calendar` Moduls verwenden, um ein Datum in einen maßgeschneiderten Formatstring zu konvertieren:

```gleam
import gleam/calendar

let geburtstag = calendar.new(1990, 12, 28)

let geburtstag_formatiert = calendar.format(geburtstag, "{YYYY}-{MM}-{DD}")
```

Die Umwandlung von Daten in Zeichenketten in Gleam ist effizient und sicher implementiert, was die Fehleranfälligkeit dieser Operation reduziert.

## Siehe auch

- Die offizielle Gleam Dokumentation: https://gleam.run/docs/
- Weitere Beispiele und Tutorials: https://exercism.io/tracks/gleam
- Die `calendar` Modul Dokumentation: https://hexdocs.pm/gleam_stdlib/gleam/calendar.html