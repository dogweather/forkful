---
title:                "Pobieranie aktualnej daty"
html_title:           "Arduino: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Pobieranie Aktualnej Daty w C++ 

## Co i Dlaczego?

Osiąganie aktualnej daty polega na pobieraniu bieżącej daty i czasu z systemu komputera. Programiści często robia to do logowania zdarzeń, generowania sygnatur czasowych czy śledzenia postępów procedur.

## Jak to zrobić:

Nowoczesne C++ (od C++11) oferuje zintegrowane narzędzia do osiągania aktualnej daty. Poniżej znajduje się przykład użycia:

```C++
#include <iostream>
#include <chrono>
#include <ctime>

int main() 
{
    auto teraz = std::chrono::system_clock::now();
    time_t czas_teraz = std::chrono::system_clock::to_time_t(teraz);
    std::cout << "Aktualna data i czas: " << ctime(&czas_teraz) << '\n';
    return 0;
}
```

Po skompilowaniu i uruchomieniu tego kodu, wydrukuje on aktualną datę i czas w czytelnej formie.

## Głębiej:

- **Kontekst historyczny**: W przeszłości, aby uzyskać aktualną datę w C++, programiści musieli polegać na funkcjach czasu z C i manipulować danymi na niskim poziomie. Od C++11, standard udostępnia bibliotekę `<chrono>`, która upraszcza te zadania.
    
- **Alternatywy**: Oprócz `<chrono>`, niestandardowe biblioteki, takie jak Boost.DateTime, oferują również funkcje do manipulowania datą i czasem.

- **Szczegóły implementacyjne**: Funkcja `system_clock::now()` zwraca aktualny punkt czasu jako obiekt `std::chrono::time_point`. `to_time_t()` konwertuje ten punkt czasu na klasyczny `std::time_t`.

## Zobacz tez: 

1. Dokumentacja C++ na temat biblioteki `<chrono>`: https://en.cppreference.com/w/cpp/chrono
2. Dowiedz się więcej o Boost.DateTime: https://www.boost.org/doc/libs/1_76_0/doc/html/date_time.html
3. Przegląd technik obsługi czasu w C++: https://www.modernescpp.com/index.php/the-three-clocks