---
date: 2024-01-26 03:43:07.499235-07:00
description: "Hvordan: C++ tilbyr flere m\xE5ter \xE5 avrunde tall p\xE5, som `floor()`,\
  \ `ceil()`, og `round()`."
lastmod: '2024-03-13T22:44:41.094170-06:00'
model: gpt-4-0125-preview
summary: "C++ tilbyr flere m\xE5ter \xE5 avrunde tall p\xE5, som `floor()`, `ceil()`,\
  \ og `round()`."
title: Avrunding av tall
weight: 13
---

## Hvordan:
C++ tilbyr flere måter å avrunde tall på, som `floor()`, `ceil()`, og `round()`:

```C++
#include <iostream>
#include <cmath> // for avrundingsfunksjoner

int main() {
    double num = 3.14;

    std::cout << "floor: " << std::floor(num) << "\n"; // Utgang: floor: 3
    std::cout << "ceil: " << std::ceil(num) << "\n";   // Utgang: ceil: 4
    std::cout << "round: " << std::round(num) << "\n"; // Utgang: round: 3

    // For fast presisjon, som å avrunde til to desimaler:
    double precise_num = 3.146;
    double multiplikator = 100.0;
    double avrundet = std::round(precise_num * multiplikator) / multiplikator;

    std::cout << "avrundet til to desimaler: " << avrundet << "\n"; // Utgang: avrundet til to desimaler: 3.15

    return 0;
}
```

## Dykk Dypere
Før C++11, avhang avrunding av manuelle teknikker eller ikke-standardiserte biblioteker. I dag tilbyr `<cmath>` robuste metoder. `floor()` runder ned, `ceil()` runder opp, mens `round()` går til nærmeste heltall, og håndterer også tie-breaking (0.5 tilfeller) ved å runde til det jevne tallet.

Å forstå oppførselen til disse funksjonene er avgjørende; for eksempel kan negative tall gi deg problemer (`std::round(-2.5)` gir `-2.0`).

Alternativer? Å caste til et heltall etter å ha lagt til 0.5 for positive tall var en klassisk hack, men feiler med negative tall og er ikke typeagnostisk. Biblioteker som Boost kan tilby mer nyanserte tilnærminger, mens språkutvidelser eller kompilatorintrinsikker kan optimalisere for spesifikt maskinvare.

## Se også
- C++ Referanse for `<cmath>`: https://en.cppreference.com/w/cpp/header/cmath
- IEEE-standarden for flyttallaritmetikk (IEEE 754): https://ieeexplore.ieee.org/document/4610935
- Boost Numeric Conversion Library: https://www.boost.org/doc/libs/release/libs/numeric/conversion/
