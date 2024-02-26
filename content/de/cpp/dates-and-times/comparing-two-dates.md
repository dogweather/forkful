---
date: 2024-01-20 17:32:22.151859-07:00
description: "Das Vergleichen zweier Daten bedeutet, ihre chronologische Reihenfolge\
  \ zu bestimmen. Programmierer machen das, um Ereignisse zu organisieren, Deadlines\
  \ zu\u2026"
lastmod: '2024-02-25T18:49:51.247039-07:00'
model: gpt-4-1106-preview
summary: "Das Vergleichen zweier Daten bedeutet, ihre chronologische Reihenfolge zu\
  \ bestimmen. Programmierer machen das, um Ereignisse zu organisieren, Deadlines\
  \ zu\u2026"
title: Vergleich von zwei Daten
---

{{< edit_this_page >}}

## Was & Warum?
Das Vergleichen zweier Daten bedeutet, ihre chronologische Reihenfolge zu bestimmen. Programmierer machen das, um Ereignisse zu organisieren, Deadlines zu verwalten oder Zeitspannen zu berechnen.

## How to:
```C++
#include <iostream>
#include <chrono>
#include <ctime>

int main() {
    // Heutiges Datum und ein spezifisches Datum erstellen
    std::chrono::system_clock::time_point today = std::chrono::system_clock::now();
    std::tm specific_time = {};
    specific_time.tm_year = 122; // Jahre seit 1900
    specific_time.tm_mon = 9;    // Oktober, 0-basierte Monatszählung
    specific_time.tm_mday = 15;  // der 15.

    std::chrono::system_clock::time_point specific_date = std::chrono::system_clock::from_time_t(mktime(&specific_time));

    // Daten vergleichen
    if (today > specific_date) {
        std::cout << "Heute ist nach dem 15. Oktober 2022." << std::endl;
    } else if (today < specific_date) {
        std::cout << "Heute ist vor dem 15. Oktober 2022." << std::endl;
    } else {
        std::cout << "Heute ist der 15. Oktober 2022." << std::endl;
    }

    return 0;
}
```
Ausgabe könnte sein: `Heute ist nach dem 15. Oktober 2022.`

## Deep Dive:
Das Vergleichen von Daten ist grundlegend in der C++ Standardbibliothek, deren Ursprünge bis in die 1970er-Jahre zurückgehen. Heute nutzen wir `std::chrono` für moderne und genaue Zeitmessung. Vor `std::chrono` war es üblich, `std::tm` mit Funktionen wie `mktime` und `difftime` zu nutzen.

Beachten Sie, dass `std::tm` das Jahr als Jahre seit 1900 speichert und die Monate bei 0 beginnen. `std::chrono` verkapselt Zeitpunkte (`time_point`) und Zeitspannen (`duration`), was Vergleiche und Zeitrechnung verbessert.

Alternativen zu `std::chrono` sind externe Bibliotheken wie Boost.Date_Time. Aber seit C++11 und der Aufnahme von `std::chrono` in die Standardbibliothek, ist die Notwendigkeit solcher Bibliotheken gesunken.

Zum Implementieren bedenken Sie Zeitzonen und Sommer-/Winterzeit. `std::chrono` bietet keine native Unterstützung für Zeitzonen, das Handling liegt beim Programmierer.

## See Also:
- C++ Referenz zu `std::chrono`: https://en.cppreference.com/w/cpp/header/chrono
- C++ Referenz zu `std::tm`: https://en.cppreference.com/w/cpp/chrono/c/tm
- Boost.Date_Time Bibliothek: https://www.boost.org/doc/libs/release/libs/date_time/
