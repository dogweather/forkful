---
title:                "Arbeiten mit CSV-Dateien"
date:                  2024-01-19
simple_title:         "Arbeiten mit CSV-Dateien"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/working-with-csv.md"
---

{{< edit_this_page >}}

## Was & Warum?
CSV steht für "Comma-Separated Values" – Daten durch Kommas getrennt. Programmierer nutzen CSV, weil es ein einfaches Format ist, um tabellarische Daten zwischen Programmen und Systemen auszutauschen.

## How to:
Bash bietet native Tools für CSV-Operationen. Hier ein paar Beispiele:

```Bash
# Zeilen zählen
wc -l daten.csv

# Erste Spalte anzeigen
cut -d ',' -f 1 daten.csv

# Zeilen filtern, die "Muster" enthalten
grep "Muster" daten.csv

# Eine neue CSV-Datei aus der ersten und dritten Spalte erstellen
cut -d ',' -f 1,3 daten.csv > neue_daten.csv

# Summe einer numerischen Spalte berechnen (hier Spalte 2)
awk -F',' '{sum += $2} END {print sum}' daten.csv
```

Beispielausgabe für `wc -l daten.csv`:

```Bash
42 daten.csv
```

## Deep Dive
CSV wurde in den frühen 70ern populär. Es ist nicht standardisiert, was Probleme mit Zeichenkodierung und Datenformaten bringen kann. Alternativen sind JSON oder XML, die strukturierter sind. Bei der Arbeit mit CSV in Bash muss man auf Textverarbeitungstools wie `cut`, `awk`, `grep`, `sort`, etc. zurückgreifen, die flexibel aber nicht fehlerverzeihend sind.

## See Also
- GNU Coreutils Dokumentation: https://www.gnu.org/software/coreutils/manual/coreutils.html
- AWK Benutzerhandbuch: https://www.gnu.org/software/gawk/manual/gawk.html
- Einführung in sed: https://www.gnu.org/software/sed/manual/sed.html
