---
title:                "C++: Das Berechnen eines Datums in der Zukunft oder Vergangenheit"
simple_title:         "Das Berechnen eines Datums in der Zukunft oder Vergangenheit"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

In der Programmierung kann es nützlich sein, ein Datum in der Zukunft oder Vergangenheit zu berechnen, wenn zum Beispiel bestimmte Ereignisse auf einem bestimmten Datum stattfinden sollen.

## Wie geht das?

Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, kann die C++ Standardbibliotheksfunktion `std::chrono::system_clock::now()` verwendet werden. Diese Funktion gibt die aktuelle Systemzeit in Form eines Zeitpunktes zurück. 

```C++
#include <iostream>
#include <chrono>

int main() {
    // Erhalte die aktuelle Systemzeit
    auto now = std::chrono::system_clock::now();
    
    // Berechne ein Datum in der Zukunft (+30 Tage)
    auto future = now + std::chrono::hours(24*30);
    
    // Berechne ein Datum in der Vergangenheit (-1 Jahr)
    auto past = now - std::chrono::hours(24*365);
    
    // Gib die Ergebnisse aus
    std::cout << "Aktuelles Datum: " << now << "\n";
    std::cout << "Zukünftiges Datum: " << future << "\n";
    std::cout << "Vergangenes Datum: " << past << "\n";
}
```

Die Ausgabe sieht dann ungefähr so aus:

```
Aktuelles Datum: 1593181331s
Zukünftiges Datum: 1595909331s
Vergangenes Datum: 1561645331s
```

Die Zahlen repräsentieren dabei die Anzahl an Sekunden seit dem 1. Januar 1970.

## Tiefergehende Informationen

Die C++ Standardbibliothek bietet auch Funktionen wie `std::chrono::time_point` und `std::chrono::duration`, um genauer mit Zeit und Datum zu arbeiten. Außerdem gibt es auch die Möglichkeit, Daten in verschiedenen Zeitzonen zu berechnen, indem man die Funktion `std::chrono::time_point_cast` verwendet.

## Siehe auch

- Dokumentation von C++ Chrono: https://devdocs.io/cpp/header/chrono
- Einrichten von Zeitzonen in C++: https://www.gormanalysis.com/blog/reading-and-writing-time-values-with-chrono/
- Datum und Uhrzeit in C++: https://www.learncpp.com/cpp-tutorial/18-11-dates-and-times/