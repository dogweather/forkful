---
date: 2024-01-26 03:43:34.234392-07:00
description: "Att avrunda tal inneb\xE4r att justera ett v\xE4rde till n\xE4rmaste\
  \ heltal eller angiven precision. Utvecklare g\xF6r detta f\xF6r att f\xF6renkla,\
  \ anpassa sig till\u2026"
lastmod: '2024-03-13T22:44:38.203959-06:00'
model: gpt-4-0125-preview
summary: "Att avrunda tal inneb\xE4r att justera ett v\xE4rde till n\xE4rmaste heltal\
  \ eller angiven precision."
title: Avrundning av tal
weight: 13
---

## Hur man gör:
C++ erbjuder flera sätt att avrunda tal, som `floor()`, `ceil()`, och `round()`:

```C++
#include <iostream>
#include <cmath> // för avrundningsfunktioner

int main() {
    double num = 3.14;

    std::cout << "floor: " << std::floor(num) << "\n"; // Utdata: floor: 3
    std::cout << "ceil: " << std::ceil(num) << "\n";   // Utdata: ceil: 4
    std::cout << "round: " << std::round(num) << "\n"; // Utdata: round: 3

    // För fast precision, såsom avrundning till två decimaler:
    double precise_num = 3.146;
    double multiplier = 100.0;
    double rounded = std::round(precise_num * multiplier) / multiplier;

    std::cout << "avrundad till två decimaler: " << rounded << "\n"; // Utdata: avrundad till två decimaler: 3.15

    return 0;
}
```

## Fördjupning
Innan C++11, berodde avrundning på manuella tekniker eller icke-standardiserade bibliotek. Idag erbjuder `<cmath>` robusta metoder. `floor()` avrundar nedåt, `ceil()` avrundar uppåt, medan `round()` går till närmaste heltal, även hantering av utjämningsfall (0.5-fall) genom avrundning till det jämna numret.

Att förstå beteendet hos dessa funktioner är avgörande; till exempel, negativa tal kan ställa till det (`std::round(-2.5)` ger `-2.0`).

Alternativ? Att typomvandla till ett int efter att ha lagt till 0.5 för positiva tal var en klassisk hackning men misslyckas med negativa tal och är inte typagnostisk. Bibliotek som Boost kan erbjuda mer nyanserade angreppssätt, medan språkutökningar eller kompilatorinbyggda funktioner kan optimera för specifik hårdvara.

## Se också
- C++ Referens för `<cmath>`: https://en.cppreference.com/w/cpp/header/cmath
- IEEE-standard för flyttalsaritmetik (IEEE 754): https://ieeexplore.ieee.org/document/4610935
- Boost Numeric Conversion Library: https://www.boost.org/doc/libs/release/libs/numeric/conversion/
