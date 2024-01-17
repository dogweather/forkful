---
title:                "Arbeiten mit CSV"
html_title:           "Bash: Arbeiten mit CSV"
simple_title:         "Arbeiten mit CSV"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/working-with-csv.md"
---

{{< edit_this_page >}}

## Was & Warum?

CSV, kurz für "Comma-separated values" oder "Durch Kommata getrennte Werte", ist ein Dateiformat, das zur Darstellung von tabellarischen Daten verwendet wird. Programmierer nutzen CSV-Dateien, um Daten zu speichern, zu verarbeiten und zu analysieren.

## Wie geht's?

Das Arbeiten mit CSV-Dateien in Bash ist eine relativ einfache Aufgabe. Mit dem Befehl `cut` können wir bestimmte Spalten aus der CSV-Datei extrahieren und mit `grep` können wir nach bestimmten Werten oder Mustern suchen.

Beispiel: Angenommen, wir haben eine CSV-Datei mit dem Namen "students.csv" und folgendem Inhalt:

```Bash
Name,Alter,Note
John,25,1.3
Maria,23,2.0
Max,21,2.7
```

Um alle Namen auszugeben, könnten wir folgenden Befehl verwenden:

```Bash
cut -d "," -f 1 students.csv
```

Dieser Befehl gibt die erste Spalte der CSV-Datei aus, wobei als Trennzeichen das Komma (`-d ","`) angegeben wird und mit `-f 1` die erste Spalte ausgewählt wird. Das Ergebnis sieht dann so aus:

```Bash
Name
John
Maria
Max
```

Um nur nach den Studierenden zu suchen, die eine Note schlechter als 2.0 haben, könnten wir folgenden Befehl verwenden:

```Bash
grep -v "Name\|John\|Maria" students.csv
```

Dieser Befehl sucht nach allen Zeilen, in denen "Name", "John" oder "Maria" nicht vorkommen. Das Ergebnis sieht dann so aus:

```Bash
Max,21,2.7
```

## Tiefere Einblicke

CSV-Dateien existieren bereits seit den 1970er Jahren und sind immer noch ein sehr beliebtes Format für den Austausch von Daten. Es gibt auch alternative Formate wie JSON oder XML, die von bestimmten Programmiersprachen bevorzugt werden. 

Für noch komplexere Aufgaben bietet Bash zudem die Möglichkeit, CSV-Dateien mit Hilfe von Schleifen und Bedingungen zu verarbeiten. Mit der integrierten Funktion `read` können wir zeilenweise auf die Daten zugreifen und diese weiterverarbeiten.

## Siehe auch

Für weitere Informationen zum Arbeiten mit CSV-Dateien in Bash empfehlen wir folgende Quellen:

- Offizielle Dokumentation zu [cut](https://www.gnu.org/software/coreutils/manual/html_node/cut-invocation.html) und [grep](https://www.gnu.org/software/grep/manual/grep.html)
- Stack Overflow Beitrag zum [Lesen von CSV-Dateien in Bash](https://stackoverflow.com/questions/13675900/easiest-way-to-read-a-csv-in-bash)
- Eine Übersicht über [verschiedene Datenformate](https://www.datacamp.com/community/tutorials/json-data-python) und ihre Verwendung in der Programmierung.