---
title:                "Das aktuelle Datum erhalten"
html_title:           "C: Das aktuelle Datum erhalten"
simple_title:         "Das aktuelle Datum erhalten"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Aktuelles Datum in C bekommen

## Was & Warum?
Das Aktuelle Datum in C zu bekommen bedeutet, das heutige Datum als Teil eines Programms zu erhalten. Programmierer tun dies, um Funktionen wie Zeitstempel, Datumserfassung und andere Zeitbezogene Verarbeitungen zu implementieren.

## So geht's:
Verwende die <time.h> Bibliothek und die Funktion `time()` um die Anzahl der vergangenen Sekunden seit dem 1. Januar 1970 zu erhalten. Verwende dann `localtime()` um diesen Wert in ein Datum und eine Zeit umzuwandeln. Hier ist ein Beispiel:

```C
#include <stdio.h>
#include <time.h>

int main()
{
    time_t now;
    time(&now);
    struct tm* my_time = localtime(&now);
    printf("Das aktuelle Datum ist: %d.%d.%d", my_time->tm_mday, my_time->tm_mon+1, my_time->tm_year+1900);
    return 0;
}
```
Erwartete Ausgabe:
`Das aktuelle Datum ist: (Tag).(Monat).(Jahr)`

## Tiefer eintauchen:
- Die Verwendung der <time.h> Bibliothek geht auf die UNIX Zeit- und Datumanzeige zurück, aus der das Datum als Anzahl der vergangenen Sekunden seit dem 1. Januar 1970 berechnet wird.
- Eine alternative Möglichkeit, das aktuelle Datum in C zu bekommen, ist die Verwendung der <ctime> Bibliothek und der Funktion `gmtime()`, die das Datum in einer anderen Struktur zurückgibt.
- Um das aktuelle Datum als String zu erhalten, kann die `strftime()` Funktion verwendet werden, die das Datum in einem spezifischen Format ausgibt.

## Siehe auch:
- [time.h documentation](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [ctime documentation](https://www.tutorialspoint.com/c_standard_library/ctime.htm)