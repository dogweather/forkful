---
title:                "Den aktuellen Datum abrufen"
aliases: - /de/cpp/getting-the-current-date.md
date:                  2024-02-03T19:09:13.924319-07:00
model:                 gpt-4-0125-preview
simple_title:         "Den aktuellen Datum abrufen"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?
Das Abrufen des aktuellen Datums in C++ ist eine grundlegende Aufgabe für Programme, die Daten verarbeiten oder anzeigen müssen, die auf der Systemuhr basieren. Es ist wesentlich für das Logging, Zeitstempeln, das Planen von Aufgaben und jede Funktionalität, die auf Daten und Zeit angewiesen ist.

## Wie:
C++ bietet mehrere Möglichkeiten, das aktuelle Datum zu erhalten, einschließlich der C++-Standardbibliothek und Drittanbieter-Bibliotheken wie Boost. Die folgenden Beispiele zeigen, wie man diese Aufgabe bewältigen kann.

### Verwendung von `<chrono>` (C++20 und später)
C++20 führte mehr Funktionalitäten in der `<chrono>`-Bibliothek ein, was es unkompliziert macht, das aktuelle Datum zu erhalten:
```cpp
#include <iostream>
#include <chrono>
#include <format> // Für std::format (C++20)

int main() {
    auto aktueller_zeitpunkt = std::chrono::system_clock::now(); // Erfasse die aktuelle Zeit
    auto aktuelle_zeit_t = std::chrono::system_clock::to_time_t(aktueller_zeitpunkt); // Konvertiere zu time_t

    // Formatieren der Zeit zu einem lesbaren Format
    std::cout << "Aktuelles Datum: " << std::format("{:%Y-%m-%d}", std::chrono::system_clock::to_time_t(aktueller_zeitpunkt)) << std::endl;

    return 0;
}
```
**Beispielausgabe:**
```plaintext
Aktuelles Datum: 2023-03-15
```

### Verwendung von `<ctime>`
Für Programmierer, die mit älteren Versionen von C++ arbeiten oder die traditionelle C-Bibliothek bevorzugen:
```cpp
#include <iostream>
#include <ctime>

int main() {
    std::time_t t = std::time(0); // Hole die aktuelle Zeit
    std::tm* jetzt = std::localtime(&t);
    std::cout << "Aktuelles Datum: " 
              << (jetzt->tm_year + 1900) << '-' 
              << (jetzt->tm_mon + 1) << '-'
              <<  jetzt->tm_mday
              << std::endl;

    return 0;
}
```
**Beispielausgabe:**
```plaintext
Aktuelles Datum: 2023-03-15
```

### Verwendung von Boost Date_Time
Für Projekte, die die Boost-Bibliotheken nutzen, bietet die Boost Date_Time-Bibliothek eine alternative Methode, um das aktuelle Datum zu erhalten:
```cpp
#include <iostream>
#include <boost/date_time.hpp>

int main() {
    // Hole den aktuellen Tag mit Boosts Gregorianischem Kalender
    boost::gregorian::date heute = boost::gregorian::day_clock::local_day();
    std::cout << "Aktuelles Datum: " << heute << std::endl;

    return 0;
}
```
**Beispielausgabe:**
```plaintext
Aktuelles Datum: 2023-Mar-15
```
Diese Beispiele bieten eine grundlegende Basis für die Arbeit mit Daten in C++, was für eine breite Palette von Anwendungen entscheidend ist.
