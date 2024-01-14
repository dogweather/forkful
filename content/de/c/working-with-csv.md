---
title:                "C: Arbeiten mit csv"
simple_title:         "Arbeiten mit csv"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/working-with-csv.md"
---

{{< edit_this_page >}}

## Warum

CSV ist ein gängiges Format, das zur Speicherung und Übertragung von Daten verwendet wird. Es ist besonders nützlich für Programmierer, die mit Tabellenkalkulationen arbeiten, da es einfach zu lesen und zu erstellen ist. Durch die Verarbeitung von CSV-Daten können Programmierer große Mengen an Informationen organisieren und analysieren.

## Wie Geht's

Um mit CSV in C zu arbeiten, müssen wir zunächst die entsprechenden Bibliotheken einbinden:

```C
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
```

Als nächstes müssen wir eine Funktion erstellen, die die CSV-Datei öffnet und die Daten in unser Programm lädt:

```C
void read_csv(char* filename) {
    FILE *csv_file = fopen(filename, "r"); // Öffnen der CSV-Datei
    char line[1000]; // Maximale Länge einer CSV-Zeile

    while (fgets(line, 1000, csv_file) != NULL) { // Lesen jeder Zeile der CSV-Datei
        int i = 0; // Index für line
        char *value; // Speichert einzelne Werte aus jeder Zeile

        value = strtok(line, ","); // Teilt die Zeile an jeder Komma-Stelle und weist das Ergebnis zu value zu

        while (value != NULL) {
            printf("%s\n", value); // Gibt den Wert aus
            value = strtok(NULL, ","); // Weist value den nächsten Wert in der Zeile zu
        }
    }

    fclose(csv_file);
}
```

Um die Funktion auszuführen, rufen wir sie in unserem Hauptprogramm auf und übergeben den Dateinamen als Parameter:

```C
int main() {
    read_csv("beispiel.csv"); // Aufruf der Funktion mit dem Dateinamen "beispiel.csv"

    return 0;
}
```

Die Ausgabe sieht dann wie folgt aus:

```
Name
Alter
Hobbys
```

```
Max
25
Schwimmen, Lesen, Reisen
```

```
Anna
28
Tennis, Fotografie, Wandern
```

## Tieferer Einblick

CSV-Dateien können in verschiedenen Formaten vorliegen, z.B. mit oder ohne Überschriften oder mit unterschiedlichen Trennzeichen. Beim Lesen und Verarbeiten von CSV-Daten ist es wichtig, die genauen Formate zu kennen und ggf. Anpassungen an unserem Code vorzunehmen.

Zudem können CSV-Dateien sehr große Datensätze enthalten, die möglicherweise nicht komplett in den Arbeitsspeicher passen. In diesem Fall sollte man darauf achten, den Speicher effizient zu nutzen und Daten möglicherweise in kleinen Chargen zu verarbeiten.

Zu guter Letzt ist es auch wichtig, Fehler beim Lesen oder Verarbeiten von CSV-Daten zu beachten und entsprechende Maßnahmen zu ergreifen, um das Programm nicht zum Absturz zu bringen.

## Siehe Auch

- [C-Programmiergrundlagen für Anfänger](https://www.geeksforgeeks.org/c-programming-language/)

- [Offizielle Dokumentation für CSV-Bibliotheken in C](https://www.jp1.eu/en/csv/)