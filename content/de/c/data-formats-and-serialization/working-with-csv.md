---
title:                "Arbeiten mit CSV"
aliases:
- /de/c/working-with-csv.md
date:                  2024-02-03T18:11:52.995548-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeiten mit CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Im Bereich der Programmierung beinhaltet die Arbeit mit CSV-Dateien (Comma-Separated Values, d.h. durch Kommas getrennte Werte) das Lesen von und das Schreiben auf Textdateien, die zeilenweise organisiert sind, wobei jede Zeile einen Datensatz repräsentiert und die Felder jedes Datensatzes durch Kommas getrennt sind. Programmierer manipulieren CSV-Dateien, um die Datenimport/-export zwischen verschiedenen Systemen zu erleichtern, aufgrund ihrer breiten Unterstützung und Einfachheit bei der Speicherung von tabellarischen Daten.

## Wie geht das:

### CSV-Dateien lesen
Um eine CSV-Datei in C zu lesen, verwenden wir Standard-Datei-I/O-Funktionen zusammen mit Zeichenkettenmanipulationsfunktionen, um jede Zeile zu parsen. Unten finden Sie ein grundlegendes Beispiel dafür, wie eine CSV-Datei gelesen und die Felder jeder Zeile auf der Konsole ausgegeben werden.

```c
#include <stdio.h>
#include <string.h>

int main() {
    FILE *fp = fopen("data.csv", "r");
    if (!fp) {
        printf("Datei kann nicht geöffnet werden\n");
        return 1;
    }

    char buf[1024];
    while (fgets(buf, 1024, fp)) {
        char *field = strtok(buf, ",");
        while(field) {
            printf("%s\n", field);
            field = strtok(NULL, ",");
        }
    }

    fclose(fp);
    return 0;
}
```
Beispiel `data.csv`:
```
Name,Alter,Beruf
John Doe,29,Softwareingenieur
```

Beispielausgabe:
```
Name
Alter
Beruf
John Doe
29
Softwareingenieur
```

### In CSV-Dateien schreiben
Ähnlich beinhaltet das Schreiben in eine CSV-Datei die Verwendung von `fprintf`, um Daten in einem durch Kommas getrennten Format zu speichern.

```c
#include <stdio.h>

int main() {
    FILE *fp = fopen("output.csv", "w");
    if (!fp) {
        printf("Datei kann nicht geöffnet werden\n");
        return 1;
    }

    char *header[] = {"Name", "Alter", "Beruf", NULL};
    for (int i = 0; header[i] != NULL; i++) {
        fprintf(fp, "%s%s", header[i], (header[i+1] != NULL) ? "," : "\n");
    }
    fprintf(fp, "%s,%d,%s\n", "Jane Doe", 27, "Datenwissenschaftlerin");

    fclose(fp);
    return 0;
}
```

Beispiel `output.csv` Inhalt:
```
Name,Alter,Beruf
Jane Doe,27,Datenwissenschaftlerin
```

## Vertiefung

Das CSV-Format, obwohl scheinbar unkompliziert, bringt seine Nuancen mit, wie beispielsweise den Umgang mit Kommas innerhalb von Feldern und das Einschließen von Feldern in Anführungszeichen. Die gezeigten grundlegenden Beispiele berücksichtigen solche Komplexitäten nicht und behandeln mögliche Fehler nicht robust.

Historisch gesehen war die Arbeit mit CSV in C hauptsächlich manuell wegen der niedrigen Ebene der Sprache und des Mangels an eingebauten, hochstufigen Abstraktionen für solche Aufgaben. Dieses manuelle Management beinhaltet das Öffnen von Dateien, das Lesen von Zeilen, das Aufteilen von Zeichenketten und das Umwandeln von Datentypen nach Bedarf.

Obwohl die direkte Manipulation von CSV-Dateien in C wertvolle Lernerfahrungen im Umgang mit Datei-I/O und Zeichenkettenverarbeitung bietet, versprechen mehrere moderne Alternativen Effizienz und weniger fehleranfällige Prozesse. Bibliotheken wie `libcsv` und `csv-parser` bieten umfassende Funktionen zum Lesen und Schreiben von CSV-Dateien, einschließlich Unterstützung für zitierte Felder und benutzerdefinierte Trennzeichen.

Alternativ, wenn man in Ökosystemen arbeitet, die dies unterstützen, kann die Integration mit Sprachen oder Plattformen, die hochstufige CSV-Manipulationsfunktionen bereitstellen (wie Python mit seiner `pandas`-Bibliothek), ein produktiverer Weg für Anwendungen sein, die eine umfangreiche CSV-Verarbeitung erfordern. Dieser sprachübergreifende Ansatz nutzt die Leistungsfähigkeit und die Fähigkeiten von C in der Systemprogrammierung, während er die Benutzerfreundlichkeit anderer Sprachen für spezifische Aufgaben wie die CSV-Verarbeitung nutzt.
