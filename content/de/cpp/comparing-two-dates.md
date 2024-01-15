---
title:                "Vergleich von zwei Daten."
html_title:           "C++: Vergleich von zwei Daten."
simple_title:         "Vergleich von zwei Daten."
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum

Vergleiche zwischen zwei Daten können in der Programmierung nützlich sein, zum Beispiel um zu überprüfen, ob ein Ereignis in der Zukunft oder Vergangenheit liegt, oder um zu bestimmen, welches Datum später ist.

## Wie geht's

Um zwei Daten miteinander zu vergleichen, gibt es in C++ die `std::chrono` Bibliothek, die eine Vielzahl an Funktionen und Variablen für die Arbeit mit Zeit und Datum bereitstellt.

```C++
#include <iostream>
#include <chrono>

using namespace std;
using namespace std::chrono;

int main() {
    // Erstelle zwei Daten-Objekte
    system_clock::time_point date1 = system_clock::now();
    system_clock::time_point date2 = system_clock::now() + hours(24);

    // Vergleiche die Objekte
    if (date1 > date2) {
        cout << "date1 ist später als date2" << endl;
    } else if (date1 < date2) {
        cout << "date1 ist früher als date2" << endl;
    } else {
        cout << "beide Daten sind gleich" << endl;
    }

    return 0;
}
```

Die Ausgabe des obigen Codes wäre:

```
date1 ist früher als date2
```

In diesem Beispiel wird die Funktion `now()` verwendet, um das aktuelle Datum und die Uhrzeit als `time_point` Objekt der Systemuhr zurückzugeben. Mit `hours()` kann ein gewünschter Zeitraum angegeben werden, um das zweite Datum entsprechend zu verschieben. Dann werden die beiden `time_point` Objekte miteinander verglichen und je nach Bedingung eine entsprechende Ausgabe gemacht.

Weitere nützliche Funktionen zum Vergleichen von Zeiten sind `time_since_epoch()`, das die Anzahl der vergangenen Zeit seit dem 1. Januar 1970 zurückgibt, und `time_since()`, das die Differenz zwischen zwei Zeitpunkten berechnet.

## Tief eintauchen

Die `std::chrono` Bibliothek verwendet ein System von Dauern und Zeitpunkten, um Zeit und Datum darzustellen. Eine Dauer ist eine Zeitspanne, die in Ticks angegeben wird, während ein Zeitpunkt ein bestimmtes Datum und eine bestimmte Uhrzeit repräsentiert. Dieses System ermöglicht eine präzise und zuverlässige Arbeit mit Zeit und Datum in C++.

Es gibt auch spezielle Datentypen wie `system_clock`, `steady_clock` und `high_resolution_clock`, die die Möglichkeit bieten, die Systemzeit, eine konstante Zeit und eine hochauflösende Zeit zu messen, um die Genauigkeit der Zeitmessung zu verbessern.

## Siehe auch

- [std::chrono Referenz](https://en.cppreference.com/w/cpp/chrono)
- [C++ Datum und Zeit Tutorial](https://www.learncpp.com/cpp-tutorial/8-13-a-simple-date-class/)
- [C++ Vergleichsoperatoren](https://www.learncpp.com/cpp-tutorial/relational-operators-and-comparisons/)