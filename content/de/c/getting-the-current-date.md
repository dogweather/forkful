---
title:                "C: Das heutige Datum erhalten"
simple_title:         "Das heutige Datum erhalten"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum
Beim Programmieren ist es oft notwendig, das aktuelle Datum zu erfassen. Das kann verwendet werden, um eine Datei mit dem aktuellen Datum zu benennen oder um einen Zeitstempel für bestimmte Aktionen zu setzen.

## Wie
Es gibt mehrere Wege, das aktuelle Datum in C zu bekommen. Ein einfacher Weg ist die Verwendung der Funktion `time()`, welche die aktuelle Zeit in Sekunden seit dem 1. Januar 1970 zurückgibt. Um das Datum in einem lesbaren Format zu erhalten, muss diese Zahl in die Struktur `struct tm` umgewandelt werden mit der Funktion `localtime()`.

```C
#include <stdio.h>
#include <time.h>

int main()
{
    // Aktuelle Zeit in Sekunden seit 1. Januar 1970
    time_t current_time;
    time(&current_time);

    // Umwandlung in die Struktur struct tm
    struct tm *local = localtime(&current_time);

    // Ausgabe des Datums in einem lesbaren Format
    printf("Das aktuelle Datum ist: %d/%d/%d\n", local->tm_mday, local->tm_mon + 1, local->tm_year + 1900);
    
    return 0;
}
```

Die Ausgabe sieht dann beispielsweise wie folgt aus:

```
Das aktuelle Datum ist: 23/04/2021
```

## Deep Dive
Die Funktion `time()` ruft in Wahrheit die Funktion `time64()` auf, welche in 64-bit Systemen die Anzahl an Millisekunden seit dem 1. Januar 1601 zurückgibt. Erst mit der Konvertierung in Sekunden und der Verwendung der Struktur `struct tm` wird das aktuelle Datum in ein lesbare Form gebracht.

Es gibt auch eine ähnliche Funktion namens `clock()` welche die CPU-Zeit seit dem Programmstart zurückgibt. Diese kann verwendet werden, wenn man die Ausführungszeit eines bestimmten Programmteils messen möchte.

## Siehe auch
- [Offizielle Dokumentation zu time()](https://www.cplusplus.com/reference/ctime/time/)
- [Weitere Informationen zur Struktur struct tm](https://www.geeksforgeeks.org/time-h-header-file-in-c-with-examples/)