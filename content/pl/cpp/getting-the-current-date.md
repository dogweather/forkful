---
title:                "Pobieranie aktualnej daty"
date:                  2024-01-20T15:13:15.358492-07:00
simple_title:         "Pobieranie aktualnej daty"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
## Co i dlaczego?
Pobieranie aktualnej daty to sposób na odczytanie bieżącego czasu systemu. Programiści wykorzystują to do logowania, wygaśnięcia sesji, time-stamping i innych funkcji zależnych od czasu.

## How to:
## Jak to zrobić:

```C++
#include <iostream>
#include <chrono>
#include <ctime>

int main() {
    // Zdobądź aktualny czas systemowy jako czas w sekundach od epoki
    auto now = std::chrono::system_clock::now();
    // Przekształć czas na typ tm dla czytelności
    std::time_t now_c = std::chrono::system_clock::to_time_t(now);
    // Wyświetl aktualną datę i czas
    std::cout << "Current date and time: " << std::ctime(&now_c) << std::endl;
    return 0;
}
```

Sample output:
```
Current date and time: Wed Mar 10 14:26:07 2023
```

## Deep Dive:
## W głąb tematu:

Historia pobierania daty sięga początków programowania. W C++ z epoki przed-standardowej, programiści często polegali na funkcjach związanych z systemem operacyjnym. Za czasów C++03 używano `std::time` z biblioteki `<ctime>`. Od C++11, `std::chrono` zapewnia bardziej precyzyjne i portable metody.

Alternatywy obejmują używanie pliku nagłówkowego `<ctime>`, który dostarcza `std::strftime` do formatowania daty i czasu. Zawansowane zastosowania mogą wymagać bibliotek zewnętrznych jak Boost.DateTime.

W implementacji `std::chrono`, czas jest reprezentowany jako punkty i trwania. Punkty to momenty w czasie, a trwania to różnice pomiędzy punktami. `std::chrono::system_clock::now()` zwraca aktualny punkt w czasie, a `std::time_t` to typ używany do przedstawienia czasu w sekundach od tzw. epoki Unixowej (początek 1970 roku).

## See Also:
## Zobacz również:

- [cppreference.com, `std::chrono`](https://en.cppreference.com/w/cpp/chrono)
- [cppreference.com, `std::ctime`](https://en.cppreference.com/w/cpp/header/ctime)
- [Boost.DateTime documentation](https://www.boost.org/doc/libs/1_75_0/doc/html/date_time.html)
