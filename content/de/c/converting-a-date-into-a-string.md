---
title:                "Datum in einen String umwandeln"
date:                  2024-01-20T17:35:55.888844-07:00
model:                 gpt-4-1106-preview
simple_title:         "Datum in einen String umwandeln"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Datum in String umwandeln bedeutet, ein Datum von einem Format, das für die Maschine lesbar ist, in Textform zu überführen, den Menschen leicht verstehen können. Programmierer machen das, um Daten benutzerfreundlich anzuzeigen oder zu speichern.

## So geht’s:
```c
#include <stdio.h>
#include <time.h>

int main() {
    time_t jetzt = time(NULL);
    struct tm *tm_struct = localtime(&jetzt);

    char datumString[100];
    strftime(datumString, sizeof(datumString), "%d.%m.%Y - %H:%M", tm_struct);
    
    printf("Aktuelles Datum und Uhrzeit: %s\n", datumString);
    return 0;
}
```
Beispiel-Ausgabe:
```
Aktuelles Datum und Uhrzeit: 23.03.2023 - 15:45
```

## Tiefgang:
Früher nutzte man in C `sprintf` und Stringmanipulation, um Datums-Strings zu erstellen. Das war umständlich und fehleranfällig. Die `strftime`-Funktion aus der Standardbibliothek `time.h` bietet eine bessere Alternative, da sie Formatier-Optionen direkt unterstützt. Diese Funktion lässt dich das Format benutzerdefinieren, samt Lokalisation für verschiedensprachige Ausgaben.

Alternativen:
- `sprintf` und manuelle Stringzusammensetzung
- Externe Bibliotheken wie `date.h` für komplexere Anforderungen

Implementierungsdetails:
- `${time_t}` ist ein Datentyp, der die Zeit seit dem sogenannten Unix-Epoch (1. Januar 1970) in Sekunden misst.
- `localtime` konvertiert `time_t` in eine `tm`-Struktur, die Datum und Uhrzeit repräsentiert.
- `strftime` formt diese Struktur basierend auf Formatparametern zu einem String um.

## Siehe auch:
- C Standardbibliothek Dokumentation: [time.h](https://en.cppreference.com/w/c/chrono)
- GNU C Bibliothek Manual zu `strftime`: [strftime](https://www.gnu.org/software/libc/manual/html_node/Formatting-Calendar-Time.html#index-strftime)
- Tiefere Einführung in Zeitfunktionen in C: [C Time Funktionen Tutorial](https://www.tutorialspoint.com/c_standard_library/time_h.htm)