---
date: 2024-01-20 18:03:03.368814-07:00
description: "Starta ett nytt projekt betyder att kicka ig\xE5ng en helt fr\xE4sch\
  \ kodkodbas fr\xE5n nollpunkt. Programmerare g\xF6r detta f\xF6r att f\xF6rverkliga\
  \ nya id\xE9er, l\xF6sa unika\u2026"
lastmod: '2024-03-13T22:44:38.209805-06:00'
model: gpt-4-1106-preview
summary: "Starta ett nytt projekt betyder att kicka ig\xE5ng en helt fr\xE4sch kodkodbas\
  \ fr\xE5n nollpunkt."
title: "Att p\xE5b\xF6rja ett nytt projekt"
weight: 1
---

## How to:
Börja ett nytt C++-projekt i modern stil kan vara så enkelt som följande:

```C++
#include <iostream>

int main() {
    std::cout << "Hej, nytt projekt!" << std::endl;
    return 0;
}
```

Kör detta och din konsol borde visa:

```
Hej, nytt projekt!
```

Men för större projekt, använd ett byggsystem, exempelvis CMake:

```CMake
cmake_minimum_required(VERSION 3.10)
project(HejProjekt)

set(CMAKE_CXX_STANDARD 17)

add_executable(HejProjekt main.cpp)
```

Din `main.cpp` kan vara densamma som ovan. Skapa sedan en byggkatalog, kör `cmake` och `make` kommandon:

```
mkdir build && cd build
cmake ..
make
./HejProjekt
```

Du får samma utskrift, nu med ett ordentligt byggsystem på plats.

## Deep Dive:
C++ har kommit en lång väg sedan det skapades av Bjarne Stroustrup på 1980-talet. Tidigare handlade det om manuella makefiler, men moderna verktyg som CMake och pakethanterare som Conan förenklar projektstart och underhåll.

För att skriva modern C++-kod använd `#include <iostream>` för in-/utmatning. Användningen av `std::endl` istället för `\n` spolar bufferten, vilket kan vara långsammare men också säkrare.

Alternativ för att starta projekt inkluderar att använda IDE:er som Visual Studio, Qt Creator eller CLion som erbjuder grafiska gränssnitt och projektguider.

## See Also:
- CMake dokumentation: https://cmake.org/documentation/
- Conan, C/C++ pakethanterare: https://conan.io/
- Modern C++ features guide: https://github.com/AnthonyCalandra/modern-cpp-features
- Bjarne Stroustrups hemsida för djupare C++ historia: http://www.stroustrup.com/
