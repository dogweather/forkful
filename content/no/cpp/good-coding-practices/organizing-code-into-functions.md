---
date: 2024-01-26 01:10:10.997260-07:00
description: "\xC5 dele opp koden i funksjoner betyr at du hugger koden din opp i\
  \ mindre, gjenbrukbare deler. Vi gj\xF8r det for \xE5 unng\xE5 gjentagelser, gj\xF8\
  re koden v\xE5r lesbar,\u2026"
lastmod: '2024-03-13T22:44:41.105450-06:00'
model: gpt-4-1106-preview
summary: "\xC5 dele opp koden i funksjoner betyr at du hugger koden din opp i mindre,\
  \ gjenbrukbare deler. Vi gj\xF8r det for \xE5 unng\xE5 gjentagelser, gj\xF8re koden\
  \ v\xE5r lesbar,\u2026"
title: Organisering av kode i funksjoner
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å dele opp koden i funksjoner betyr at du hugger koden din opp i mindre, gjenbrukbare deler. Vi gjør det for å unngå gjentagelser, gjøre koden vår lesbar, og forenkle feilsøking og testing. Godt organiserte funksjoner kan være som å ha en boks med pent merkede verktøy, klar til bruk og deling.

## Hvordan:
La oss ta en vanlig oppgave: å beregne arealet av en sirkel. I stedet for å skrive samme formel hver gang, kapsler vi den inn i en funksjon.

```C++
#include <iostream>
#define PI 3.14159

double calculateCircleArea(double radius) {
    return PI * radius * radius;
}

int main() {
    double r = 5.0;
    std::cout << "Arealet av en sirkel med radius " << r << " er " << calculateCircleArea(r) << std::endl;
    return 0;
}
```

Eksempel på utdata:
```
Arealet av en sirkel med radius 5 er 78.5397
```

## Dypdykk
Historisk sett var prosedyrer og funksjoner ryggraden i strukturert programmering, fremmet på 1960-tallet for å bekjempe problemer med "spagettikode" i tidligere imperativ programmeringsspråk. Alternativer som OOP (Objektorientert Programmering) tar dette videre ved å assosiere disse funksjonene med datastrukturer. I C++ har du vanlige funksjoner, klassemetoder (inkludert statiske metoder), lambdaer og mal-funksjoner, som hver tilbyr forskjellige fordeler. Implementering av godt organiserte funksjoner medfører vanligvis å følge prinsipper som DRY ("Don't Repeat Yourself") og SRP (Single Responsibility Principle), som betyr at hver funksjon gjør én ting bare og gjør det godt.

## Se også
For mer om funksjoner i C++:
- https://en.cppreference.com/w/cpp/language/functions
- https://www.learncpp.com/cpp-tutorial/77-introduction-to-functions/

For designprinsipper relatert til funksjoner:
- https://en.wikipedia.org/wiki/Single-responsibility_principle
- https://en.wikipedia.org/wiki/Don%27t_repeat_yourself

Lær om lambdaer og avansert bruk av funksjoner:
- https://www.cprogramming.com/c++11/c++11-lambda-closures.html
- https://isocpp.org/wiki/faq/cpp14-language#lambda-captures
