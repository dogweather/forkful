---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:25.428983-07:00
description: "Pobieranie bie\u017C\u0105cej daty w C++ jest podstawowym zadaniem dla\
  \ program\xF3w, kt\xF3re musz\u0105 przetwarza\u0107 lub wy\u015Bwietla\u0107 daty\
  \ w oparciu o zegar systemowy. Jest to\u2026"
lastmod: '2024-03-13T22:44:35.723348-06:00'
model: gpt-4-0125-preview
summary: "Pobieranie bie\u017C\u0105cej daty w C++ jest podstawowym zadaniem dla program\xF3\
  w, kt\xF3re musz\u0105 przetwarza\u0107 lub wy\u015Bwietla\u0107 daty w oparciu\
  \ o zegar systemowy. Jest to\u2026"
title: Pobieranie aktualnej daty
weight: 29
---

## Czym i dlaczego?
Pobieranie bieżącej daty w C++ jest podstawowym zadaniem dla programów, które muszą przetwarzać lub wyświetlać daty w oparciu o zegar systemowy. Jest to niezbędne do logowania, znakowania czasu, planowania zadań i wszelkiej funkcjonalności, która opiera się na datach i czasie.

## Jak to zrobić:
C++ oferuje kilka sposobów na uzyskanie bieżącej daty, w tym standardową bibliotekę C++ i biblioteki stron trzecich, takie jak Boost. Poniższe przykłady pokazują, jak to osiągnąć.

### Korzystając z `<chrono>` (C++20 i nowsze)
C++20 wprowadził więcej funkcjonalności w bibliotece `<chrono>`, ułatwiając uzyskanie bieżącej daty:
```cpp
#include <iostream>
#include <chrono>
#include <format> // Dla std::format (C++20)

int main() {
    auto current_time_point = std::chrono::system_clock::now(); // Złap bieżący czas
    auto current_time_t = std::chrono::system_clock::to_time_t(current_time_point); // Konwersja na time_t

    // Formatuj czas do czytelnego formatu
    std::cout << "Aktualna Data: " << std::format("{:%Y-%m-%d}", std::chrono::system_clock::to_time_t(current_time_point)) << std::endl;

    return 0;
}
```
**Przykładowy Wynik:**
```plaintext
Aktualna Data: 2023-03-15
```

### Korzystając z `<ctime>`
Dla programistów pracujących ze starszymi wersjami C++ lub tych, którzy preferują tradycyjną bibliotekę C:
```cpp
#include <iostream>
#include <ctime>

int main() {
    std::time_t t = std::time(0); // Pobierz bieżący czas
    std::tm* now = std::localtime(&t);
    std::cout << "Aktualna Data: " 
              << (now->tm_year + 1900) << '-' 
              << (now->tm_mon + 1) << '-'
              <<  now->tm_mday
              << std::endl;

    return 0;
}
```
**Przykładowy Wynik:**
```plaintext
Aktualna Data: 2023-03-15
```

### Korzystając z Boost Date_Time
Dla projektów, które wykorzystują biblioteki Boost, biblioteka Boost Date_Time oferuje alternatywną metodę na uzyskanie bieżącej daty:
```cpp
#include <iostream>
#include <boost/date_time.hpp>

int main() {
    // Pobierz bieżący dzień korzystając z kalendarza gregoriańskiego Boost
    boost::gregorian::date today = boost::gregorian::day_clock::local_day();
    std::cout << "Aktualna Data: " << today << std::endl;

    return 0;
}
```
**Przykładowy Wynik:**
```plaintext
Aktualna Data: 2023-Mar-15
```
Te przykłady zapewniają podstawową wiedzę na temat pracy z datami w C++, która jest kluczowa dla szerokiego zakresu zastosowań.
