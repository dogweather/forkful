---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:03.501681-07:00
description: "Stringinterpolatie gaat over het invoegen van variabelen in strings.\
  \ We doen dit om ter plekke berichten samen te stellen, output te personaliseren\
  \ of\u2026"
lastmod: '2024-03-13T22:44:51.098868-06:00'
model: gpt-4-0125-preview
summary: "Stringinterpolatie gaat over het invoegen van variabelen in strings. We\
  \ doen dit om ter plekke berichten samen te stellen, output te personaliseren of\u2026"
title: Een string interpoleren
weight: 8
---

## Wat & Waarom?
Stringinterpolatie gaat over het invoegen van variabelen in strings. We doen dit om ter plekke berichten samen te stellen, output te personaliseren of dynamische queries op te bouwen.

## Hoe:
C++ heeft geen ingebouwde stringinterpolatie zoals sommige andere talen. Je gebruikt vaak `std::ostringstream`, `std::format` (vanaf C++20), of printf-stijl formattering.

Met `std::ostringstream`:
```cpp
#include <sstream>
#include <iostream>

int main() {
    std::ostringstream bericht;
    int leeftijd = 30;
    bericht << "Hallo, ik ben " << leeftijd << " jaar oud.";
    std::cout << bericht.str() << std::endl; // "Hallo, ik ben 30 jaar oud."
}
```

Met `std::format` (C++20):
```cpp
#include <format>
#include <iostream>

int main() {
    int leeftijd = 30;
    std::string bericht = std::format("Hallo, ik ben {} jaar oud.", leeftijd);
    std::cout << bericht << std::endl; // "Hallo, ik ben 30 jaar oud."
}
```

## Diepgaand Onderzoek
Voor C++20, concateneerden we strings met streams of sprintf, wat omslachtig was. Met de komst van `std::format`, halen we eindelijk de moderne talen zoals Python in met hun f-strings.

`std::ostringstream`: Dit biedt ons een stream-achtige manier om strings op te bouwen. Het is veelzijdig maar niet het meest bondig. Het is al jaren de go-to vanwege zijn veiligheid en gebruiksgemak.

`std::format`: Geïntroduceerd in C++20, biedt het Python-achtige formattering. Het is leesbaarder en efficiënter dan streamconcatenatie maar vereist nieuwere compilers.

Alternatieven bestaan zoals Boost.Format of het gebruik van stringconcatenatie, maar deze zijn niet zo schoon of kunnen overhead met zich meebrengen.

Stringinterpolatie is versuikering, maar het is zoet. Het vereenvoudigt de code en vermijdt de prestatiehit van herhaaldelijk aan strings toevoegen.

## Zie Ook
- [cppreference over std::format](https://en.cppreference.com/w/cpp/utility/format)
- [cppreference over std::ostringstream](https://en.cppreference.com/w/cpp/io/basic_ostringstream)
- [Boost.Format Bibliotheek](https://www.boost.org/doc/libs/release/libs/format/)
