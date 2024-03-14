---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:43.986461-07:00
description: "Die Arbeit mit CSV-Dateien (Comma Separated Values) umfasst das Parsen,\
  \ Manipulieren und Generieren von Daten in einem tabellarischen Format, das weit\u2026"
lastmod: '2024-03-13T22:44:54.331116-06:00'
model: gpt-4-0125-preview
summary: "Die Arbeit mit CSV-Dateien (Comma Separated Values) umfasst das Parsen,\
  \ Manipulieren und Generieren von Daten in einem tabellarischen Format, das weit\u2026"
title: Arbeiten mit CSV
---

{{< edit_this_page >}}

## Was & Warum?

Die Arbeit mit CSV-Dateien (Comma Separated Values) umfasst das Parsen, Manipulieren und Generieren von Daten in einem tabellarischen Format, das weit verbreitet für den Datenaustausch zwischen Anwendungen genutzt wird. Programmierer führen diese Operationen durch, um Daten effizient zu verarbeiten und zu analysieren, Aufgaben zu automatisieren oder sich mit anderen Systemen zu integrieren.

## Wie:

Fish Shell verfügt von sich aus nicht über speziell für die CSV-Manipulation entwickelte Funktionen. Allerdings können Sie Unix-Dienstprogramme wie `awk`, `sed` und `cut` für grundlegende Operationen nutzen oder spezialisierte Tools wie `csvkit` für fortgeschrittenere Aufgaben verwenden.

### Eine CSV-Datei lesen und die erste Spalte ausdrucken:
Verwendung von `cut` zur Extraktion der ersten Spalte:
```fish
cut -d ',' -f1 data.csv
```
Beispielausgabe:
```
Name
Alice
Bob
```

### Filtern von CSV-Zeilen anhand des Spaltenwerts:
Verwendung von `awk` zum Finden von Zeilen, bei denen die zweite Spalte "42" entspricht:
```fish
awk -F, '$2 == "42" { print $0 }' data.csv
```
Beispielausgabe:
```
Bob,42,London
```

### Eine CSV-Datei modifizieren (z.B. Hinzufügen einer Spalte):
Verwendung von `awk` zum Hinzufügen einer Spalte mit einem statischen Wert "NeueSpalte":
```fish
awk -F, 'BEGIN {OFS=","} {print $0,"NeueSpalte"}' data.csv > modified.csv
```
Beispielausgabe in `modified.csv`:
```
Name,Alter,Stadt,NeueSpalte
Alice,30,New York,NeueSpalte
Bob,42,London,NeueSpalte
```

### Verwendung von `csvkit` für fortgeschrittenere Operationen:
Stellen Sie zunächst sicher, dass Sie `csvkit` installiert haben. Falls nicht, installieren Sie es mit pip: `pip install csvkit`.

**Konvertieren einer CSV-Datei in JSON:**
```fish
csvjson data.csv > data.json
```
Beispiel `data.json` Ausgabe:
```json
[{"Name":"Alice","Alter":"30","Stadt":"New York"},{"Name":"Bob","Alter":"42","Stadt":"London"}]
```

**Filtern mit `csvkit`'s `csvgrep`:**
```fish
csvgrep -c 2 -m 42 data.csv
```
Dieser Befehl repliziert die Filteraufgabe, verwendet aber `csvkit`, zielt auf Spalte 2 mit dem Wert "42".

Abschließend lässt sich sagen, dass, während Fish Shell selbst vielleicht keine direkten CSV-Manipulationsfähigkeiten bietet, dessen nahtlose Integration mit Unix-Dienstprogrammen und die Verfügbarkeit von Tools wie `csvkit` leistungsstarke Optionen für die Arbeit mit CSV-Dateien bieten.
