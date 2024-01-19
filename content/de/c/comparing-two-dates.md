---
title:                "Vergleich von zwei Daten"
html_title:           "C#: Vergleich von zwei Daten"
simple_title:         "Vergleich von zwei Daten"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Was und Warum?
Das Vergleichen von zwei Daten ermöglicht die Bestimmung der zeitlichen Abfolge von Ereignissen. Programmierer machen dies, weil es in vielen Anwendungen notwendig ist, einschließlich Datenmanagement, Algorithmen und User Experience Design.

## So geht's:
Mit der `difftime()` Funktion von C können wir zwei `time_t` Werte vergleichen. Hier ist ein einfaches Beispiel:

```C
#include <stdio.h>
#include <time.h>

int main() {
    time_t now;
    time(&now);

    struct tm newyear;
    newyear = *localtime(&now);

    newyear.tm_hour = 0;    
    newyear.tm_min = 0;     
    newyear.tm_sec = 0;
    newyear.tm_mon = 0;     
    newyear.tm_mday = 1;   

    double seconds = difftime(now,mktime(&newyear));

    printf("%.f Sekunden seit Neujahr sind verstrichen.\n", seconds);

    return 0;
}
```
Beispiel-Ausgabe könnte sein:
```
16042082 Sekunden seit Neujahr sind verstrichen.
```

## Tiefere Einblicke
Historisch gesehen hatte jede Programmiersprache ihre eigene Methode zum Vergleichen von Daten. In C verwenden wir die Funktion `difftime()`, die die Differenz in Sekunden zwischen zwei `time_t` Werten zurückgibt.

Eine Alternativmethode in C wäre die Verwendung der Funktion `mktime()`, um die Daten in einen einheitlichen `time_t` Wert zu konvertieren und dann direkt zu vergleichen.

Bedenken Sie, dass `difftime()` die Genauigkeit nur bis zur Sekunde gewährleistet. Wenn Sie eine genauere Zeitspanne benötigen, müssen Sie möglicherweise andere Bibliotheken oder Funktionen verwenden.

## Siehe auch
1. [C Standard Library time.h](https://en.cppreference.com/w/c/chrono): Eine umfassende Referenz zur C Standardbibliothek für die Zeitverarbeitung.
2. [Time Manipulation in C](https://www.tutorialspoint.com/c_standard_library/c_function_difftime.htm): Ein Einsteigertutorial zu Zeitmanipulation in C.
3. [GNU C Library Documentation](http://www.gnu.org/software/libc/manual/html_node/Date-and-Time.html): Eine tiefergehende Dokumentation zur Verwendung von Datum und Zeit in der GNU C Bibliothek.