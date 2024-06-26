---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:58.270575-07:00
description: "Hvordan: C++ tilbyr flere m\xE5ter \xE5 f\xE5 tak i den n\xE5v\xE6rende\
  \ datoen p\xE5, inkludert C++ standardbiblioteket og tredjeparts biblioteker som\
  \ Boost. F\xF8lgende\u2026"
lastmod: '2024-03-13T22:44:41.110448-06:00'
model: gpt-4-0125-preview
summary: "C++ tilbyr flere m\xE5ter \xE5 f\xE5 tak i den n\xE5v\xE6rende datoen p\xE5\
  , inkludert C++ standardbiblioteket og tredjeparts biblioteker som Boost."
title: "F\xE5 dagens dato"
weight: 29
---

## Hvordan:
C++ tilbyr flere måter å få tak i den nåværende datoen på, inkludert C++ standardbiblioteket og tredjeparts biblioteker som Boost. Følgende eksempler demonstrerer hvordan man kan utføre denne oppgaven.

### Bruk av `<chrono>` (C++20 og senere)
C++20 introduserte flere funksjonaliteter i `<chrono>`-biblioteket, noe som gjør det enkelt å få tak i den nåværende datoen:
```cpp
#include <iostream>
#include <chrono>
#include <format> // For std::format (C++20)

int main() {
    auto current_time_point = std::chrono::system_clock::now(); // Fang opp det nåværende tidspunktet
    auto current_time_t = std::chrono::system_clock::to_time_t(current_time_point); // Konverter til time_t

    // Formatter tiden til et lesbart format
    std::cout << "Nåværende Dato: " << std::format("{:%Y-%m-%d}", std::chrono::system_clock::to_time_t(current_time_point)) << std::endl;

    return 0;
}
```
**Eksempelutskrift:**
```plaintext
Nåværende Dato: 2023-03-15
```

### Bruk av `<ctime>`
For programmerere som arbeider med eldre versjoner av C++ eller de som foretrekker det tradisjonelle C-biblioteket:
```cpp
#include <iostream>
#include <ctime>

int main() {
    std::time_t t = std::time(0); // Hent nåværende tid
    std::tm* nå = std::localtime(&t);
    std::cout << "Nåværende Dato: " 
              << (nå->tm_year + 1900) << '-' 
              << (nå->tm_mon + 1) << '-'
              <<  nå->tm_mday
              << std::endl;

    return 0;
}
```
**Eksempelutskrift:**
```plaintext
Nåværende Dato: 2023-03-15
```

### Bruk av Boost Date_Time
For prosjekter som benytter Boost-bibliotekene, tilbyr Boost Date_Time-biblioteket en alternativ metode for å få tak i den nåværende datoen:
```cpp
#include <iostream>
#include <boost/date_time.hpp>

int main() {
    // Hent den nåværende dagen ved hjelp av Boosts Gregorianske kalender
    boost::gregorian::date i dag = boost::gregorian::day_clock::local_day();
    std::cout << "Nåværende Dato: " << i dag << std::endl;

    return 0;
}
```
**Eksempelutskrift:**
```plaintext
Nåværende Dato: 2023-Mar-15
```
Disse eksemplene gir en grunnleggende basis for arbeid med datoer i C++, avgjørende for et bredt spekter av applikasjoner.
