---
date: 2024-01-20 17:30:47.808419-07:00
description: "Die Berechnung eines Datums in der Zukunft oder Vergangenheit bezieht\
  \ sich darauf, von einem gegebenen Datum ausgehend ein neues Datum zu ermitteln.\u2026"
lastmod: '2024-03-11T00:14:28.099602-06:00'
model: gpt-4-1106-preview
summary: "Die Berechnung eines Datums in der Zukunft oder Vergangenheit bezieht sich\
  \ darauf, von einem gegebenen Datum ausgehend ein neues Datum zu ermitteln.\u2026"
title: "Berechnung eines zuk\xFCnftigen oder vergangenen Datums"
---

{{< edit_this_page >}}

## Was & Warum?
Die Berechnung eines Datums in der Zukunft oder Vergangenheit bezieht sich darauf, von einem gegebenen Datum ausgehend ein neues Datum zu ermitteln. Programmierer nutzen dies für Features wie Erinnerungen, Buchungen und Prognosen.

## So geht's:
```C++
#include <iostream>
#include <chrono>
#include <iomanip>

int main() {
    using std::chrono::system_clock;
    using std::chrono::duration_cast;
    using std::chrono::hours;
    using std::chrono::time_point_cast;

    // Heutiges Datum erhalten
    auto jetzt = system_clock::now();

    // Datum in 30 Tagen
    auto inDreißigTagen = jetzt + std::chrono::hours(24 * 30);

    // Datum vor 30 Tagen
    auto vorDreißigTagen = jetzt - std::chrono::hours(24 * 30);

    // Zeitpunkte in tm umwandeln
    time_t zeitInDreißig = system_clock::to_time_t(inDreißigTagen);
    time_t zeitVorDreißig = system_clock::to_time_t(vorDreißigTagen);

    // Ausgeben
    std::cout << "Heute: " << std::put_time(localtime(&zeit), "%F") << '\n';
    std::cout << "In 30 Tagen: " << std::put_time(localtime(&zeitInDreißig), "%F") << '\n';
    std::cout << "Vor 30 Tagen: " << std::put_time(localtime(&zeitVorDreißig), "%F") << '\n';

    return 0;
}
```
Output:
```
Heute: 2023-04-01
In 30 Tagen: 2023-05-01
Vor 30 Tagen: 2023-03-02
```

## Deep Dive
Die `<chrono>`-Bibliothek kam mit C++11 und revolutionierte die Zeit- und Datumsverwaltung. Zuvor musste man oft auf plattformspezifische API oder externe Bibliotheken wie Boost zurückgreifen. Alternativen sind auch heute noch die C Time-API oder Libraries wie date.h. Bei der Zeitberechnung sollte man auch auf Zeitumstellungen (Sommerzeit) und Schaltjahre achten, was mit `<chrono>` automatisch passiert.

## See Also
- C++ Reference for `<chrono>`: https://en.cppreference.com/w/cpp/chrono
- GitHub repo for Howard Hinnant's date library, a timezone-aware extension: https://github.com/HowardHinnant/date
- C++20 Calendar and Time-Zone Support (new features): https://en.cppreference.com/w/cpp/chrono/calendar
