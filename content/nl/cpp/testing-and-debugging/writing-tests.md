---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:39.406062-07:00
description: "Het schrijven van tests controleert of je code doet wat het moet doen,\
  \ en vangt bugs vroeg op. Programmeurs testen om tijd, hoofdpijn te besparen en\
  \ om\u2026"
lastmod: 2024-02-19 22:05:10.203288
model: gpt-4-0125-preview
summary: "Het schrijven van tests controleert of je code doet wat het moet doen, en\
  \ vangt bugs vroeg op. Programmeurs testen om tijd, hoofdpijn te besparen en om\u2026"
title: Tests Schrijven
---

{{< edit_this_page >}}

## Wat & Waarom?
Het schrijven van tests controleert of je code doet wat het moet doen, en vangt bugs vroeg op. Programmeurs testen om tijd, hoofdpijn te besparen en om betrouwbaarheid te garanderen.

## Hoe:
Laten we een eenvoudige C++ functie en een test met het Catch2 framework gebruiken.

```cpp
// main.cpp
#define CATCH_CONFIG_MAIN  // Laat Catch main() voorzien.
#include <catch2/catch.hpp>

int Add(int a, int b) {
    return a + b;
}

TEST_CASE( "Optellen werkt", "[wiskunde]" ) {
    REQUIRE( Add(2, 2) == 4 );
}
```
Compileer met `g++ -std=c++17 main.cpp -o test -lcatch2` en voer uit met `./test`. Voorbeelduitvoer:

```
Alle tests geslaagd (1 bewering in 1 testgeval)
```

## Diepgaande Duik
Testen was niet altijd de norm. In de jaren '70 was het handmatig. Nu zijn geautomatiseerde tests essentieel in agile en TDD (Test-Driven Development). Alternatieven voor Catch2? Google Test, Boost.Test, en CppUnit, elk met unieke kenmerken. Onthoud: tests beoordelen of code aan de vereisten voldoet, niet of die vereisten correct zijn - dat is een specificatiekwestie.

## Zie Ook
- Catch2: https://github.com/catchorg/Catch2
- Google Test: https://github.com/google/googletest
- Boost.Test: https://www.boost.org/doc/libs/release/libs/test/
- CppUnit: https://freedesktop.org/wiki/Software/cppunit/
