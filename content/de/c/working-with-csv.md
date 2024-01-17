---
title:                "Arbeiten mit csv"
html_title:           "C: Arbeiten mit csv"
simple_title:         "Arbeiten mit csv"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/working-with-csv.md"
---

{{< edit_this_page >}}

## Was & Warum?
CSV ist eine Textdatei-Format, das verwendet wird, um tabellarische Daten zu speichern. Programmierer nutzen es, um Daten zu organisieren und auszutauschen, da es einfach zu lesen und zu schreiben ist.

## Wie geht's?
Um mit CSV in C zu arbeiten, müssen Sie zuerst die Funktionen `fopen()` und `fclose()` verwenden, um die Datei zu öffnen und zu schließen. Dann können Sie die Daten mithilfe von `fscanf()` aus der Datei lesen.

```C
FILE *fp;
fp = fopen("beispiel.csv", "r"); // Öffnen der CSV-Datei
if (fp == NULL) {
  printf("Fehler beim Öffnen der Datei!");
  return -1;
}

char name[20];
int alter;
double gehalt;

while (fscanf(fp, "%s,%d,%lf", name, &alter, &gehalt) != EOF) { // Lesen der Daten aus der CSV-Datei
  printf("Name: %s\nAlter: %d\nGehalt: %.2lf\n", name, alter, gehalt);
}

fclose(fp); // Schließen der CSV-Datei
```

### Tiefer tauchen
CSV steht für "Comma-Separated Values" und wurde in den 1970er Jahren entwickelt. Es gibt auch alternative Formate wie TSV (Tab-Separated Values) und DSV (Delimiter-Separated Values). Die Implementierung von CSV in C kann durch Verwendung von Bibliotheken wie `libcsv` oder `libc-csv` erleichtert werden.

## Siehe auch
- Informationen über CSV: https://de.wikipedia.org/wiki/CSV_(Dateiformat)
- Verwendung von CSV in C mit `libcsv`: https://github.com/ashenm/libcsv
- Beispielprogramm mit `libc-csv`: https://android.googlesource.com/platform/bionic/+/5c51612/libc/csv/csv_read.c