---
title:                "Vergleich von zwei Daten"
html_title:           "C: Vergleich von zwei Daten"
simple_title:         "Vergleich von zwei Daten"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Vergleichen von zwei Daten ist eine häufige Aufgabe in der Programmierung. Es beinhaltet das Überprüfen, ob zwei angegebene Daten identisch oder unterschiedlich sind. Programmierer nutzen diese Anweisung, um Bedingungen zu überwachen und bei Bedarf bestimmte Aktionen auszuführen.

## Anleitung:
```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main()
{
    //Erstes Datum angeben
    struct tm start = {0};
    start.tm_year = 2021 - 1900;
    start.tm_mon = 6;
    start.tm_mday = 1;

    //Zweites Datum angeben
    struct tm end = {0};
    end.tm_year = 2021 - 1900;
    end.tm_mon = 6;
    end.tm_mday = 5;

    //Vergleichen
    if (mktime(&start) == mktime(&end))
    {
        printf("Die Daten sind identisch.");
    }
    else
    {
        printf("Die Daten sind unterschiedlich.");
    }

    return 0;
}
```
### Ausgabe:
Die Daten sind unterschiedlich.

## Tiefgehende Informationen:
Das Vergleichen von Daten hat sich im Laufe der Zeit weiterentwickelt und gehört zu den grundlegenden Aufgaben in der Programmierung. Abhängig von der verwendeteten Programmiersprache können alternative Methoden zur Dateivergleichung wie Endereignisse oder Dateiattribute verwendet werden. Die Implementierung des Dateivergleichs erfordert die Umwandlung der Daten und den Vergleich der entsprechenden Werte.

## Siehe auch:
- [C Standard Library time.h](https://www.studytonight.com/c/headers-in-c/time.h)
- [C Tutorial: How to compare dates in C](https://www.youtube.com/watch?v=Q8D1-o7v2aI)
- [Alternative Methoden zur Dateivergleichung in C](http://www.gnu.org/software/libc/manual/html_node/Opening-and-Closing-Files.html)