---
date: 2024-01-20 17:52:05.650822-07:00
description: "Hur man g\xF6r: Sample Output."
lastmod: '2024-04-05T21:53:39.549602-06:00'
model: gpt-4-1106-preview
summary: ''
title: "Skriva ut fels\xF6kningsdata"
weight: 33
---

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
