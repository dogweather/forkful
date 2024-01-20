---
title:                "Berechnung eines Datums in der Zukunft oder Vergangenheit"
html_title:           "C: Berechnung eines Datums in der Zukunft oder Vergangenheit"
simple_title:         "Berechnung eines Datums in der Zukunft oder Vergangenheit"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Ein Datum in der Zukunft oder Vergangenheit berechnen mit C 

## Was & Warum? 
In der Programmierung bezieht sich das Berechnen eines Datums in der Zukunft oder Vergangenheit auf die Manipulation von Zeitstempeln. Es ist wichtig für diverse Anwendungen, wie etwa Planungssysteme, Kalender-Apps oder zeitabhängige Simulationen.

## So geht's:
Die C Standardbibliothek bietet Funktionen wie `mktime` und `localtime`, um mit Daten zu arbeiten. Hier ist ein Beispiel zur Berechnung eines Daten eine Woche in der Zukunft.

```C
#include <stdio.h>
#include <time.h>

int main() {
    time_t jetzt;
    struct tm zeitstruktur;

    // Aktuelle Zeit holen
    time(&jetzt);

    // Aktuelle lokale Zeit konvertieren
    zeitstruktur = *localtime(&jetzt);

    // Eine Woche in die Zukunft gehen
    zeitstruktur.tm_mday += 7;

    // Wieder in einen Zeitstempel konvertieren
    jetzt = mktime(&zeitstruktur);

    printf("Date in one week: %s", ctime(&jetzt));

    return 0;
}
```

Ausgabesample: `Date in one week: Mon Mar 1 10:23:42 2027`

## Vertiefung 
Historisch gesehen war der Umgang mit Zeit und Datum in C eine Herausforderung, vor allem wegen der unterschiedlichen Konventionen weltweit. Die Einführung der `<time.h>` Bibiliothek, die Funktionen wie `mktime` und `localtime` bietet, hat das allerdings vereinfacht.

Es gibt auch alternative Möglichkeiten, wie die Nutzung von Drittanbieter-Bibliotheken (z.B. Boost.Date_Time für C++) oder den Einsatz der C++ Standardbibliothek (`<chrono>`), wenn die Interoperabilität mit C++ Codes notwendig ist.

Ein wichtiger Punkt ist, dass `mktime` und `localtime` die Zeitzone des Systems nutzen. Wenn Sie mit Zeitzonen arbeiten müssen, benötigen Sie weitere Lösungen, wie z.B. die Nutzung der `tzset` Funktion oder Drittanbieter-Libraries.

## Siehe auch

Für mehr Informationen und weitere Beispiele, schauen Sie hier:

- Cplusplus.com, "Time library": [Link](http://www.cplusplus.com/reference/ctime/)
- GNU.org, "Time functions": [Link](https://www.gnu.org/software/libc/manual/html_node/Time-Functions.html)
- Stackoverflow, "Add one week to current time in C": [Link](https://stackoverflow.com/questions/3024197/what-is-the-easiest-way-to-get-the-current-time-in-current-time-milliseconds-in-c)