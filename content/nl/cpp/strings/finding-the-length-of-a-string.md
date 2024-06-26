---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:30.418697-07:00
description: "Hoe te: C++ biedt een eenvoudige manier om de lengte van een string\
  \ te vinden met de `length()` methode van de `std::string` klasse. Maar als je van\
  \ de\u2026"
lastmod: '2024-03-13T22:44:51.103629-06:00'
model: gpt-4-0125-preview
summary: C++ biedt een eenvoudige manier om de lengte van een string te vinden met
  de `length()` methode van de `std::string` klasse.
title: De lengte van een string vinden
weight: 7
---

## Hoe te:
C++ biedt een eenvoudige manier om de lengte van een string te vinden met de `length()` methode van de `std::string` klasse. Maar als je van de oude stempel bent, kun je nog steeds met C-stijl strings en `strlen()` gaan. Hier zijn beide in actie:

```C++
#include <iostream>
#include <string>
#include <cstring>

int main() {
    // Gebruik van std::string
    std::string groet = "Hallo, wereld!";
    std::cout << "Lengte van string (std::string): " << groet.length() << std::endl;

    // Gebruik van C-stijl string
    const char *c_groet = "Hallo, wereld!";
    std::cout << "Lengte van string (C-stijl): " << strlen(c_groet) << std::endl;

    return 0;
}
```

Voorbeelduitvoer:
```
Lengte van string (std::string): 13
Lengte van string (C-stijl): 13
```

## Diepere Duik:
Oorspronkelijk erfde C++ de C-stijl karakterreeksen en de bijbehorende `strlen()` functie van C. `strlen()` berekent de lengte door door de array te lopen totdat het het null-karakter, `'\0'`, tegenkomt. Dit is een eenvoudige maar effectieve strategie, maar het kan niet op tegen de efficiëntie van `std::string.length()`, dat typisch de lengte bijhoudt voor snelle ophaling.

Alternatieven? Zeker:
- Je kunt ook de `size()` methode gebruiken, identiek aan `length()` voor `std::string`.
- Voor brede karakterreeksen zijn `std::wstring` en zijn `length()` methode je vrienden.
- Pittigere keuzes omvatten aangepaste functies of het gebruik van algoritmen zoals `std::distance` met iterators.

Let op, `std::string::length()` retourneert een `size_t` type, een ondertekend geheel getal, dat je kan verrassen met onverwachte gedragingen als je het mengt met ondertekende types in uitdrukkingen.

## Zie Ook:
- C++ referentie voor `std::string::length()`: https://en.cppreference.com/w/cpp/string/basic_string/length
- C++ referentie voor `strlen()`: https://en.cppreference.com/w/cpp/string/byte/strlen
- Meer over `std::string` versus C-stijl strings: https://www.learncpp.com/cpp-tutorial/4-4a-c-style-strings/
- Voor de enthousiastelingen die dieper in de `std::string` klasse willen duiken: https://en.cppreference.com/w/cpp/string/basic_string
