---
title:                "Analiza składniowa daty z łańcucha znaków"
aliases: - /pl/cpp/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:13:55.322604-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analiza składniowa daty z łańcucha znaków"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?
Parsowanie daty ze stringa polega na interpretacji formatu ciągu znaków w celu wyodrębnienia składników daty, takich jak dzień, miesiąc i rok. Programiści robią to, aby obsłużyć dane wejściowe użytkownika, odczytać pliki danych lub współpracować z API, które komunikują daty w formatach tekstowych. Jest to istotne dla przetwarzania danych, walidacji oraz wykonywania arytmetyki dat w aplikacjach.

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
