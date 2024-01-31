---
title:                "Arbeiten mit CSV-Dateien"
date:                  2024-01-19
simple_title:         "Arbeiten mit CSV-Dateien"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/working-with-csv.md"
---

{{< edit_this_page >}}

## Was & Warum?
CSV steht für "Comma-separated values", also durch Kommas getrennte Werte. Es ist ein Format, das hauptsächlich für den Import und Export von großen Datenmengen verwendet wird. Programmierer arbeiten mit CSV, weil es universell, leicht zu lesen und zu bearbeiten ist.

## How to:
Hier ist ein einfaches Beispiel, wie man eine CSV-Datei in C liest.

```C
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
    FILE *fp = fopen("beispiel.csv", "r");
    if (!fp) {
        printf("Datei konnte nicht geöffnet werden.\n");
        return 1;
    }

    char buf[1024];
    while (fgets(buf, 1024, fp)) {
        char *field = strtok(buf, ",");
        while (field) {
            printf("%s\n", field);
            field = strtok(NULL, ",");
        }
    }

    fclose(fp);
    return 0;
}
```
Ausgabe:
```
Spalte1
Spalte2
Spalte3
...
```

## Deep Dive
CSV ist seit den frühen Computerzeiten in Gebrauch. Es ist einfach, doch nicht standardisiert, was zu unterschiedlichen Implementierungen führt. Alternativen wie JSON oder XML gibt es, aber sie sind komplexer. Im Kern geht es bei CSV um das Einlesen und Zerlegen von Zeilen und Spalten, was oft mit Funktionen wie `strtok` in C umgesetzt wird.

## See Also
- [RFC 4180](https://tools.ietf.org/html/rfc4180), das nächste, was ein Standard für CSV ist.
- [C Standard Library](http://www.cplusplus.com/reference/clibrary/), für weitere Details zu den verwendeten Funktionen.
- [SQLite](https://www.sqlite.org/index.html), um CSV in eine Datenbank zu importieren und zu bearbeiten.
