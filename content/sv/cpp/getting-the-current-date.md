---
title:                "Att hämta aktuellt datum"
date:                  2024-01-20T15:13:21.731953-07:00
html_title:           "Bash: Att hämta aktuellt datum"
simple_title:         "Att hämta aktuellt datum"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att få aktuellt datum i programmering innebär att man hämtar datumet just nu, som systemet rapporterar. Programmerare gör detta för att tidsstämpla händelser, hantera kalenderfunktioner eller bara visa datumet för användaren.

## Hur man gör:
Kolla på det här enkla exemplet med C++20:

```C++
#include <iostream>
#include <chrono>
#include <format>

int main() {
    auto current_time = std::chrono::system_clock::now();
    std::time_t time_now = std::chrono::system_clock::to_time_t(current_time);
    
    // Klassiskt sätt att visa datum
    std::cout << std::ctime(&time_now);

    // Med C++20 kan du använda std::format för ett mer kontrollerat format
    auto local_time = *std::localtime(&time_now);
    std::cout << std::format("{:%Y-%m-%d}", local_time) << std::endl;

    return 0;
}
```

Output:
```
Tue Mar 14 12:45:23 2023
2023-03-14
```

## Fördjupning
För att hämta nuvarande datum i C++, använd `chrono` biblioteket som introducerades i C++11 och har utökats i C++20. Historiskt sett hade programmerare begränsningar kring tid och datum, och lösningar var beroende av operativsystemets funktioner och C-bibliotek.

Alternativa sätt att hantera datum och tid inkluderar `ctime` och POSIX-bibliotek. C++20 medförde `std::format`, som förenklar formatering av datum och text.

Detaljerna i att hämta och representera tid är komplexa på grund av tidszoner, sommartid och andra systemspecifika detaljer. C++ tar hand om dessa genom `std::chrono` och `std::localtime` för plattformsoberoende användning.

## Se även
- [cppreference.com](https://en.cppreference.com/w/cpp/chrono) för mer info om `<chrono>`
- [fmtlib](https://fmt.dev/latest/index.html) för dokumentation om `std::format` i C++20
- [cplusplus.com](http://www.cplusplus.com/reference/ctime/localtime/) för info om `std::localtime`
