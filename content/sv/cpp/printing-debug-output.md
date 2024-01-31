---
title:                "Skriva ut felsökningsdata"
date:                  2024-01-20T17:52:05.650822-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skriva ut felsökningsdata"

category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Utskrift för felsökning är när man skriver ut meddelanden för att förstå vad programmet gör. Programmerare gör detta för att snabbt hitta och fixa buggar.

## Hur man gör:
```C++
#include <iostream>

int main() {
    int x = 10;
    std::cout << "Debug: x has value " << x << std::endl; // Skriver ut värdet av x
    // ... övrig kod ...
    return 0;
}
```
Sample Output:
```
Debug: x has value 10
```

## Djupdykning
Printing debug output i C++ är grundläggande men kraftfullt. Historiskt sett har `printf` använts från C, vilket fortfarande fungerar. Alternativ inkluderar loggbibliotek, som `spdlog` eller `boost::log`, och att skriva till en fil. Generella implementationer använder ofta `std::cout` för konsolen eller `std::ofstream` för filer. Preprocessor directives, som `#ifdef DEBUG`, kan hjälpa till att endast inkludera debug-utskrifter i utvecklingsversioner.

## Se Även
- C++ Reference std::cout: https://en.cppreference.com/w/cpp/io/cout
- spdlog GitHub Repository: https://github.com/gabime/spdlog
- Boost.Log Documentation: https://www.boost.org/doc/libs/1_75_0/libs/log/doc/html/index.html
