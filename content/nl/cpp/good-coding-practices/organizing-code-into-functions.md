---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:55.836549-07:00
description: "Hoe te: Laten we een veelvoorkomende taak nemen: het berekenen van de\
  \ oppervlakte van een cirkel. In plaats van elke keer dezelfde formule te schrijven,\u2026"
lastmod: '2024-03-13T22:44:51.118412-06:00'
model: gpt-4-0125-preview
summary: Laten we een veelvoorkomende taak nemen.
title: Code organiseren in functies
weight: 18
---

## Hoe te:
Laten we een veelvoorkomende taak nemen: het berekenen van de oppervlakte van een cirkel. In plaats van elke keer dezelfde formule te schrijven, encapsuleren we deze in een functie.

```C++
#include <iostream>
#define PI 3.14159

double berekenCirkelOppervlakte(double radius) {
    return PI * radius * radius;
}

int main() {
    double r = 5.0;
    std::cout << "Oppervlakte van de cirkel met radius " << r << " is " << berekenCirkelOppervlakte(r) << std::endl;
    return 0;
}
```

Voorbeelduitvoer:
```
Oppervlakte van de cirkel met radius 5 is 78.5397
```

## Diepere Duik
Historisch gezien waren procedures en functies de ruggengraat van gestructureerd programmeren, gepromoot in de jaren 60 om de problemen van "spaghetticode" in eerdere imperatieve programmeertalen aan te pakken. Alternatieven zoals OOP (Objectgeoriënteerd Programmeren) gaan verder door deze functies te associëren met gegevensstructuren. In C++ beschik je over reguliere functies, klassemethoden (inclusief statische methoden), lambdas en templatefuncties, elk met verschillende voordelen. Het implementeren van goed georganiseerde functies omvat meestal het naleven van principes zoals DRY ("Don't Repeat Yourself") en SRP (Single Responsibility Principle), wat betekent dat elke functie slechts één ding doet en dat goed doet.

## Zie Ook
Voor meer over functies in C++:
- https://en.cppreference.com/w/cpp/language/functions
- https://www.learncpp.com/cpp-tutorial/77-introduction-to-functions/

Voor ontwerpprincipes met betrekking tot functies:
- https://en.wikipedia.org/wiki/Single-responsibility_principle
- https://en.wikipedia.org/wiki/Don%27t_repeat_yourself

Leer over lambdas en geavanceerd functiegebruik:
- https://www.cprogramming.com/c++11/c++11-lambda-closures.html
- https://isocpp.org/wiki/faq/cpp14-language#lambda-captures
