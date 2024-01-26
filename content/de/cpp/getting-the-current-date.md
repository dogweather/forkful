---
title:                "Aktuelles Datum abrufen"
date:                  2024-01-20T15:13:26.344006-07:00
html_title:           "C: Aktuelles Datum abrufen"
simple_title:         "Aktuelles Datum abrufen"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Aktualdatum zu erhalten bedeutet, das momentane Datum des Systems abzufragen. Programmierer nutzen das, um datumsbezogene Funktionen zu implementieren, von einfachen Zeitstempeln bis zu wiederkehrenden Ereignissen.

## So geht's:
```C++
#include <iostream>
#include <chrono>
#include <iomanip>
#include <ctime>

int main() {
    // Hol dir die aktuelle Zeit
    auto jetzt = std::chrono::system_clock::now();
    
    // Wandele sie in eine lesbare Form um
    std::time_t end_time = std::chrono::system_clock::to_time_t(jetzt);

    // Ausgabe im Jahr-Monat-Tag Format
    std::cout << "Aktuelles Datum: " 
              << std::put_time(std::localtime(&end_time), "%Y-%m-%d") 
              << std::endl;

    return 0;
}
```
Ausgabe könnte sein:
```
Aktuelles Datum: 2023-04-01
```

## Tiefgang:
Früher nutzten viele C++ Programmierer die C Standardbibliothek Funktion `time.h` für Datums- und Zeitfunktionalitäten. Seit C++11 gibt es `std::chrono`, das für Zeitpunkt- und Dauerberechnungen genutzt wird. Alternativen umfassen Bibliotheken wie Boost.DateTime. Die `std::chrono` Bibliothek bietet eine typsichere Schnittstelle und ist vorzuziehen, um Probleme mit Zeitzonen und Sommer-/Winterzeit zu vermeiden. Einstellungen wie Zeitzonen und Lokalisierung werden in `std::put_time` berücksichtigt, um das Datum entsprechend zu formatieren.

## Siehe auch:
- CPP Reference zu `<chrono>`: https://en.cppreference.com/w/cpp/header/chrono
- CPP Reference zu `<iomanip>`: https://en.cppreference.com/w/cpp/header/iomanip
- Informationen zu Boost.DateTime: https://www.boost.org/doc/libs/release/libs/date_time/

Das waren die Grundlagen, um in C++ das aktuelle Datum abzufragen. Etwas einfaches, aber ein essentielles Werkzeug in deiner Programmierkiste.
