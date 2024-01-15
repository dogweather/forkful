---
title:                "Arbeiten mit CSV"
html_title:           "C: Arbeiten mit CSV"
simple_title:         "Arbeiten mit CSV"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/working-with-csv.md"
---

{{< edit_this_page >}}

## Warum

CSV (Comma Separated Values) ist ein weit verbreitetes Dateiformat für die Speicherung von Daten in tabellarischer Form. In der Programmierung kann es oft notwendig sein, mit CSV-Dateien zu arbeiten, sei es für den Import von Daten oder für die Ausgabe von Ergebnissen. In diesem Artikel erfahren Sie, wie Sie mit CSV-Dateien in C arbeiten können.

## Wie geht es

Zunächst müssen wir eine CSV-Datei öffnen und die Daten in unserem Programm verarbeiten. Dazu verwenden wir die fopen() Funktion, um die Datei zu öffnen, und die Funktionen fscanf() oder fgets() zum Lesen der Daten. Wir können dann die gelesenen Daten in einer Datenstruktur speichern, z. B. einem Array oder einer Linked List. Hier ist ein Beispielcode:

```C
#include <stdio.h>

int main() {
    // Öffnet die CSV-Datei zum Lesen
    FILE *csvFile = fopen("meine_datei.csv", "r");

    // Überprüft, ob die Datei erfolgreich geöffnet wurde
    if (csvFile == NULL) {
        printf("Fehler beim Öffnen der Datei!");
        return 1;
    }

    // Erstellt eine Datenstruktur zum Speichern der Daten
    // Hier verwenden wir ein Array
    int daten[100];
    int i = 0;

    // Liest die Daten aus der Datei und speichert sie im Array
    while (fscanf(csvFile, "%d,%d", &daten[i], &daten[i+1]) == 2) {
        i += 2;
    }

    // Schließt die Datei
    fclose(csvFile);

    // Gibt die Daten aus
    for (int j = 0; j < i; j += 2) {
        printf("Datenreihe %d: %d, %d\n", (j+2)/2, daten[j], daten[j+1]);
    }

    return 0;
}
```

Das obige Beispiel liest die ersten beiden Spalten jeder Zeile der CSV-Datei und speichert sie in einem Array. Sie können dies entsprechend Ihren Anforderungen anpassen.

## Deep Dive

In der Lektüre von CSV-Dateien ist es wichtig, auf die Formatierung der Daten zu achten. CSV-Dateien verwenden Kommata, um die Spalten zu trennen und Zeilenumbrüche, um die Zeilen zu trennen. Stellen Sie daher sicher, dass Ihre Daten die richtige Anzahl an Spalten haben und dass Zeichen wie Kommata oder Zeilenumbrüche in den Daten korrekt behandelt werden. Außerdem kann es hilfreich sein, einen CSV-Parser-Bibliothek zu verwenden, um die Arbeit mit CSV-Dateien zu erleichtern.

## Siehe auch

- [Die offizielle C-Dokumentation zu Dateien](https://en.cppreference.com/w/c/io)
- [Einführung in CSV-Dateien](https://www.w3schools.com/csv/)
- [Eine Liste von CSV-Parser-Bibliotheken für C](https://github.com/datasets/csv/blob/master/README.md#csv-parsers)