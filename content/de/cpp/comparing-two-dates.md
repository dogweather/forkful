---
title:                "Vergleich von zwei Daten"
html_title:           "C#: Vergleich von zwei Daten"
simple_title:         "Vergleich von zwei Daten"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Vergleichen von zwei Daten ist der Prozess, bei dem festgestellt wird, ob ein Datum vor, nach oder gleich einem anderen Datum liegt. Programmierer tun dieses zur Datensortierung, Terminplanung und für viele andere Funktionen, von Logbuch-Einträgen bis zu Alarmsystemen.

## So geht's:

In C++ können wir die Bibliothek `#include <ctime>` verwenden, um Daten zu vergleichen. Wenden wir es in ein paar Code-Beispielen an.

```C++
#include <ctime>
#include <iostream>

int main() {
    std::time_t now = std::time(0);
    tm *ltm = std::localtime(&now);
    
    int today_day = ltm->tm_mday;
    int today_month = 1 + ltm->tm_mon;
    int today_year = 1900 + ltm->tm_year;

    int other_day = 15;
    int other_month = 2;
    int other_year = 2022;

    if(other_year < today_year ||
       (other_year == today_year && other_month < today_month) ||
       (other_year == today_year && other_month == today_month && other_day < today_day))
    {
        std::cout << "Das andere Datum liegt in der Vergangenheit" << std::endl;
    }
    else if(other_year == today_year && other_month == today_month && other_day == today_day)
    {
        std::cout << "Das andere Datum ist heute" << std::endl;
    }
    else
    {
        std::cout << "Das andere Datum liegt in der Zukunft" << std::endl;
    }

    return 0;
}
```

Mögliche Ausgabe:

```
Das andere Datum liegt in der Vergangenheit
```

## Tiefgehendes

Die Datumsvergleichsfunktionen, die wir jetzt haben, sind das Ergebnis jahrzehntelanger Entwicklung. Frühe und historische Methoden beinhalteten oft komplexe Algorithmen, basierend auf handgeschriebenen Kalendergesetzen.

Für Alternativen können Sie auch die C++11 Chrono-Bibliothek verwenden, die eine große Sammlung von Funktionen zur Manipulation von Daten und Zeiten bereitstellt. Es enthält Tools zum Messen der Zeit auf Nanosekundenebene, zur Konvertierung zwischen verschiedenen Zeitzonen und zum Vergleichen von Daten.

Die in unserem Code-Beispiel verwendete Methode zur Datumsvergleichung basiert auf der Umwandlung eines `std::time_t`-Objekts in einen `tm`-Datentyp und auf dem Vergleich der einzelnen Komponenten (Tag, Monat, Jahr) von zwei Datumsobjekten.

## Siehe auch

- C++ Dokumentation zur `<ctime>` Bibliothek: (https://en.cppreference.com/w/cpp/chrono/c)
- Einführung in die C++11 Chrono-Bibliothek: (https://en.cppreference.com/w/cpp/chrono)
- Historische Datenvergleichsalgorithmen: (https://en.wikipedia.org/wiki/Calendar_algorithm)