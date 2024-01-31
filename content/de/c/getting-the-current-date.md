---
title:                "Aktuelles Datum abrufen"
date:                  2024-01-20T15:12:53.640610-07:00
html_title:           "C: Aktuelles Datum abrufen"
simple_title:         "Aktuelles Datum abrufen"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Abrufen des aktuellen Datums in C ist der Prozess, bei dem Du die momentane Datumswerte des Systems holst. Programmierer brauchen das, um Zeitstempel zu erzeugen, Datumsoperationen durchzuführen oder einfach das Datum in der Benutzeroberfläche anzuzeigen.

## So geht's:
```c
#include <stdio.h>
#include <time.h>

int main() {
    time_t now;
    time(&now);
    struct tm *local = localtime(&now);

    printf("Datum: %02d.%02d.%04d\n", local->tm_mday, local->tm_mon + 1, local->tm_year + 1900);

    return 0;
}
```
Ergebnis:
```
Datum: 15.03.2023
```

## Tiefer eintauchen:
Früher wurden für die Datums- und Zeitfunktionen in C Bibliotheken wie '<time.h>' verwendet. Diese bieten Strukturen wie `struct tm`, mit denen man lokale Zeitinformationen erhält.

Alternativ könnten Entwickler POSIX-Funktionen wie `gettimeofday` verwenden, aber für reine Datumswerte ist `localtime` einfacher und weit verbreitet.

Eine wichtige Sache beim Umgang mit Datum und Zeit in C ist, zu beachten, dass `time_t` üblicherweise Sekunden seit dem 1. Januar 1970 (die Epoche) zählt. Die `localtime`-Funktion konvertiert diese Sekunden in eine für Menschen lesbare Struktur.

Trotz der Einfachheit von `time.h` gibt es auch Probleme, wie z.B. die Y2K38-Problematik, welche auf 32-Bit-Systemen nach dem 19. Januar 2038 auftritt. Neuere Systeme und die meisten 64-Bit-Systeme sollten davon allerdings nicht betroffen sein.

Es gibt auch modernere Ansätze zur Datums/Zeit-Verwaltung in C, wie z.B. die C11 `timespec_get()`-Funktion aus '<time.h>', die präzisere Zeitangaben in Nanosekunden liefert.

## Siehe auch:
- C Standard Library - <time.h>: https://en.cppreference.com/w/c/chrono
- C11 Standard - General utilities: https://www.iso.org/standard/50372.html
- POSIX `gettimeofday`: https://pubs.opengroup.org/onlinepubs/9699919799/functions/gettimeofday.html
