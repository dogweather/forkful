---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:53.822099-07:00
description: 'Hoe te: C++ biedt meerdere manieren om getallen af te ronden, zoals
  `floor()`, `ceil()`, en `round()`.'
lastmod: '2024-03-13T22:44:51.107581-06:00'
model: gpt-4-0125-preview
summary: C++ biedt meerdere manieren om getallen af te ronden, zoals `floor()`, `ceil()`,
  en `round()`.
title: Afronden van getallen
weight: 13
---

## Hoe te:
C++ biedt meerdere manieren om getallen af te ronden, zoals `floor()`, `ceil()`, en `round()`:

```C++
#include <iostream>
#include <cmath> // voor afrondingsfuncties

int main() {
    double num = 3.14;

    std::cout << "floor: " << std::floor(num) << "\n"; // Uitvoer: floor: 3
    std::cout << "ceil: " << std::ceil(num) << "\n";   // Uitvoer: ceil: 4
    std::cout << "round: " << std::round(num) << "\n"; // Uitvoer: round: 3

    // Voor vaste precisie, zoals afronden op twee decimalen:
    double precise_num = 3.146;
    double multiplier = 100.0;
    double rounded = std::round(precise_num * multiplier) / multiplier;

    std::cout << "afgerond op twee decimalen: " << rounded << "\n"; // Uitvoer: afgerond op twee decimalen: 3.15

    return 0;
}
```

## Diepgaand
Voor C++11 was afronden afhankelijk van handmatige technieken of niet-standaard bibliotheken. Vandaag de dag biedt `<cmath>` robuuste methoden. `floor()` rondt naar beneden af, `ceil()` naar boven, terwijl `round()` naar het dichtstbijzijnde gehele getal gaat, zelfs bij het afhandelen van tie-breaks (0,5 gevallen) door af te ronden naar het even getal.

Het begrijpen van het gedrag van deze functies is cruciaal; bijvoorbeeld, negatieve getallen kunnen problemen veroorzaken (`std::round(-2.5)` resulteert in `-2.0`).

Alternatieven? Een klassieke truc was het casten naar een int na het toevoegen van 0,5 voor positieve getallen, maar dit faalt bij negatieve getallen en is niet type-agnostisch. Bibliotheken zoals Boost kunnen meer genuanceerde benaderingen bieden, terwijl taaluitbreidingen of compiler intrinsieken kunnen optimaliseren voor specifieke hardware.

## Zie Ook
- C++ Referentie voor `<cmath>`: https://en.cppreference.com/w/cpp/header/cmath
- IEEE-standaard voor floating-point rekenkunde (IEEE 754): https://ieeexplore.ieee.org/document/4610935
- Boost Numeric Conversion Library: https://www.boost.org/doc/libs/release/libs/numeric/conversion/
