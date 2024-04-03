---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:55.322604-07:00
description: "Jak to zrobi\u0107: We wsp\xF3\u0142czesnym C++ mo\u017Cna u\u017Cy\u0107\
  \ biblioteki `<chrono>` do obs\u0142ugi dat i czas\xF3w natywnie, ale nie obs\u0142\
  uguje ona bezpo\u015Brednio parsowania ze\u2026"
lastmod: '2024-03-13T22:44:35.722242-06:00'
model: gpt-4-0125-preview
summary: "We wsp\xF3\u0142czesnym C++ mo\u017Cna u\u017Cy\u0107 biblioteki `<chrono>`\
  \ do obs\u0142ugi dat i czas\xF3w natywnie, ale nie obs\u0142uguje ona bezpo\u015B\
  rednio parsowania ze string\xF3w bez r\u0119cznego parsowania dla bardziej skomplikowanych\
  \ format\xF3w."
title: "Analiza sk\u0142adniowa daty z \u0142a\u0144cucha znak\xF3w"
weight: 30
---

## Jak to zrobić:
We współczesnym C++ można użyć biblioteki `<chrono>` do obsługi dat i czasów natywnie, ale nie obsługuje ona bezpośrednio parsowania ze stringów bez ręcznego parsowania dla bardziej skomplikowanych formatów. Jednak dla formatów dat ISO 8601 i prostych niestandardowych formatów, oto jak możesz osiągnąć parsowanie.

**Korzystając z `<chrono>` i `<sstream>`:**
```cpp
#include <iostream>
#include <sstream>
#include <chrono>
#include <iomanip>

int main() {
    std::string date_str = "2023-04-15"; // format ISO 8601
    std::istringstream iss(date_str);
    
    std::chrono::year_month_day parsed_date;
    iss >> std::chrono::parse("%F", parsed_date);
    
    if (!iss.fail()) {
        std::cout << "Zinterpretowana data: " << parsed_date << std::endl;
    } else {
        std::cout << "Nie udało się zinterpretować daty." << std::endl;
    }
    
    return 0;
}
```
Przykładowe wyjście:
```
Zinterpretowana data: 2023-04-15
```

Dla bardziej złożonych formatów lub gdy mamy do czynienia ze starszymi wersjami C++, popularne są biblioteki stron trzecich jak `date.h` (biblioteka dat Howarda Hinnanta). Oto jak możesz zinterpretować różne formaty z jej pomocą:

**Korzystając z biblioteki `date.h`:**
Upewnij się, że masz zainstalowaną bibliotekę. Możesz ją znaleźć [tutaj](https://github.com/HowardHinnant/date).

```cpp
#include "date/date.h"
#include <iostream>

int main() {
    std::string date_str = "Kwiecień 15, 2023";
    
    std::istringstream iss(date_str);
    date::sys_days parsed_date;
    iss >> date::parse("%B %d, %Y", parsed_date);
    
    if (!iss.fail()) {
        std::cout << "Zinterpretowana data: " << parsed_date << std::endl;
    } else {
        std::cout << "Nie udało się zinterpretować daty z ciągu znaków." << std::endl;
    }

    return 0;
}
```
Przykładowe wyjście (może się różnić w zależności od ustawień regionalnych i daty w twoim systemie):
```
Zinterpretowana data: 2023-04-15
```
