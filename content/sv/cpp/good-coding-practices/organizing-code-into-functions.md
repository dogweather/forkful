---
date: 2024-01-26 01:09:50.086938-07:00
description: "Hur man g\xF6r: L\xE5t oss ta en vanlig uppgift: att ber\xE4kna arean\
  \ av en cirkel. Ist\xE4llet f\xF6r att skriva samma formel varje g\xE5ng, kapslar\
  \ vi in den i en\u2026"
lastmod: '2024-03-13T22:44:38.214535-06:00'
model: gpt-4-1106-preview
summary: "L\xE5t oss ta en vanlig uppgift."
title: Att organisera kod i funktioner
weight: 18
---

## Hur man gör:
Låt oss ta en vanlig uppgift: att beräkna arean av en cirkel. Istället för att skriva samma formel varje gång, kapslar vi in den i en funktion.

```C++
#include <iostream>
#define PI 3.14159

double calculateCircleArea(double radius) {
    return PI * radius * radius;
}

int main() {
    double r = 5.0;
    std::cout << "Arean av en cirkel med radien " << r << " är " << calculateCircleArea(r) << std::endl;
    return 0;
}
```

Exempel på utskrift:
```
Arean av en cirkel med radien 5 är 78.5397
```

## Fördjupning
Historiskt sett var procedurer och funktioner ryggraden i strukturerad programmering, förespråkad på 1960-talet för att bekämpa problem med "spagettikod" i tidigare imperativa programmeringsspråk. Alternativ som OOP (Objektorienterad Programmering) tar det ett steg längre genom att associera dessa funktioner med datastrukturer. I C++ har du vanliga funktioner, klassmetoder (inklusive statiska metoder), lambdas och mallfunktioner, var och en erbjuder olika fördelar. Implementering av välorganiserade funktioner innebär vanligtvis att man följer principer som DRY ("Don't Repeat Yourself") och SRP (Single Responsibility Principle), vilket innebär att varje funktion gör en sak och gör det bra.

## Se även
För mer om funktioner i C++:
- https://en.cppreference.com/w/cpp/language/functions
- https://www.learncpp.com/cpp-tutorial/77-introduction-to-functions/

För designprinciper relaterade till funktioner:
- https://en.wikipedia.org/wiki/Single-responsibility_principle
- https://en.wikipedia.org/wiki/Don%27t_repeat_yourself

Lär dig om lambdas och avancerad användning av funktioner:
- https://www.cprogramming.com/c++11/c++11-lambda-closures.html
- https://isocpp.org/wiki/faq/cpp14-language#lambda-captures
