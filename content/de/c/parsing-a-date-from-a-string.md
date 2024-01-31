---
title:                "Datum aus einem String parsen"
date:                  2024-01-20T15:34:51.467497-07:00
html_title:           "Arduino: Datum aus einem String parsen"
simple_title:         "Datum aus einem String parsen"

category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Parsen eines Datums aus einem String bedeutet, Text in ein Datum-Format umzuwandeln, das der Computer versteht. Programmierer brauchen das, um von Benutzern oder Datenquellen eingegebene Datumsangaben zu verarbeiten und sinnvoll zu nutzen.

## So geht's:
Man verwendet die `strptime` Funktion, um einen String in eine `struct tm` umzuwandeln. Hier ein Beispiel:

```C
#include <stdio.h>
#include <time.h>

int main() {
    struct tm tm;
    char *input = "2023-03-15";
    if (strptime(input, "%Y-%m-%d", &tm) != NULL) {
        printf("Erfolgreich geparst: %d-%02d-%02d\n", tm.tm_year+1900, tm.tm_mon+1, tm.tm_mday);
    } else {
        printf("Fehler beim Parsen des Datums.\n");
    }
    return 0;
}
```

Probelauf:

```
Erfolgreich geparst: 2023-03-15
```

## Tiefgang:
Früher benutzten Programmierer eigene Algorithmen, um Strings zu parsen, was fehleranfällig war. Heutzutage bieten Bibliotheken wie `time.h` fertige Lösungen. Alternativen zu `strptime` sind Bibliotheken wie `getdate` oder Programmiersprachen-interne Parser. Bei der Implementierung ist zu beachten, dass `strptime` die Locale-Einstellungen des Systems nutzt und nicht mit allen Systemen kompatibel ist.

## Siehe auch:
- C Standard Library Dokumentation: https://en.cppreference.com/w/c/chrono/strptime
- GNU C Library Dokumentation zu `strptime`: https://www.gnu.org/software/libc/manual/html_node/Low_002dLevel-Time-String-Parsing.html
- Informationen über `struct tm`: https://en.cppreference.com/w/c/chrono/tm
