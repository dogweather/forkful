---
title:                "Berechnung eines zukünftigen oder vergangenen Datums"
date:                  2024-01-20T17:28:42.961673-07:00
model:                 gpt-4-1106-preview
html_title:           "Elixir: Berechnung eines zukünftigen oder vergangenen Datums"
simple_title:         "Berechnung eines zukünftigen oder vergangenen Datums"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Datum Berechnung: Zukunft und Vergangenheit

## Was & Warum?
Datum-Berechnungen sind notwendig, um herauszufinden, welches Datum es nach oder vor einer bestimmten Zeitdauer sein wird. Entwickler nutzen solche Berechnungen für Features wie Erinnerungen, Buchungen und Gültigkeitsprüfungen in Softwareanwendungen.

## Wie geht das?
```c
#include <stdio.h>
#include <time.h>

int main() {
    time_t jetzt;
    struct tm neuesDatum;
    double tageZurAddition = 10; // 10 Tage in die Zukunft

    time(&jetzt); 
    neuesDatum = *localtime(&jetzt);
    
    printf("Aktuelles Datum und Uhrzeit: %s", asctime(&neuesDatum));

    neuesDatum.tm_mday += tageZurAddition; 
    mktime(&neuesDatum);

    printf("Datum in %f Tagen: %s", tageZurAddition, asctime(&neuesDatum));

    return 0;
}
```
### Beispieloutput
```
Aktuelles Datum und Uhrzeit: Tue Sep 08 12:00:00 2020
Datum in 10.000000 Tagen: Fri Sep 18 12:00:00 2020
```

## Tiefer Eintauchen
Historisch gesehen, sind Datum-Berechnungen komplex dank unterschiedlicher Kalendersysteme über die Jahrhunderte. In der heutigen Softwareentwicklung standardisiert die `time.h` Bibliothek Datum- und Zeitfunktionen für C-Programmierer.

Es gibt Alternativen, wie das `chrono` Modul in C++, das zusätzliche Funktionalitäten bietet. Im Embedded- und Systems-Bereich ist jedoch `time.h` und dessen Funktionen standard.

Für korrekte Berechnungen berücksichtigt `mktime` automatisch Monats- und Jahreswechsel sowie Schaltjahre. Vorsicht ist jedoch bei Zeitumstellungen und Zeitzonen benötigt, da diese zu unerwarteten Ergebnissen führen können.

## Siehe auch
- C Standard Library `time.h`: http://en.cppreference.com/w/c/chrono
- GNU C Library Dokumentation zu Datum und Zeit: https://www.gnu.org/software/libc/manual/html_node/Date-and-Time.html
- Zeitmanagement in C++ mit `chrono`: http://en.cppreference.com/w/cpp/chrono