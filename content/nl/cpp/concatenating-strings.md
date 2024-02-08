---
title:                "Samenvoegen van strings"
aliases:
- nl/cpp/concatenating-strings.md
date:                  2024-01-28T21:57:00.935512-07:00
model:                 gpt-4-0125-preview
simple_title:         "Samenvoegen van strings"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/cpp/concatenating-strings.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Stringconcatenatie is het aan elkaar rijgen van twee of meer strings achter elkaar. Programmeurs doen dit om zinnen te bouwen, berichten te creëren of invoergegevens te combineren voor verwerking of weergave.

## Hoe te:
In C++ hebben we een paar manieren om strings aan elkaar te plakken. Hier is een voorbeeld met `std::string` en de plus (`+`) operator:

```cpp
#include <iostream>
#include <string>

int main() {
    std::string hello = "Hallo, ";
    std::string world = "Wereld!";
    
    std::string groet = hello + world;
    
    std::cout << groet << std::endl; // Uitvoer: Hallo, Wereld!
    return 0;
}
```

Snel en simpel, niet? Maar, we kunnen ook `append()` gebruiken:

```cpp
#include <iostream>
#include <string>

int main() {
    std::string hello = "Hallo, ";
    hello.append("Wereld!");
    
    std::cout << hello << std::endl; // Uitvoer: Hallo, Wereld!
    return 0;
}
```

Of zelfs `operator+=` als je dat wilt:

```cpp
#include <iostream>
#include <string>

int main() {
    std::string hello = "Hallo, ";
    hello += "Wereld!";
    
    std::cout << hello << std::endl; // Uitvoer: Hallo, Wereld!
    return 0;
}
```

## Diepere Duik
Historisch gezien nam C++ het stokje over van C, dat karakterarrays en functies als `strcat()` voor stringverwerking gebruikte. Het was rommeliger en foutgevoeliger.

Modern C++ heeft de situatie verbeterd met `std::string`. Het is veiliger, gemakkelijker te lezen en biedt je opties. Als `std::string` niet jouw ding is, is er `std::stringstream` of zelfs `std::format` (vanaf C++20) voor de liefhebbers van formattering.

Onder de motorkap houdt het concatenaten van strings geheugentoewijzing en kopiëren in. Als dit onzorgvuldig gedaan wordt, kan het de prestaties van je programma zwaar treffen. Slimme pointers en move-semantiek verlichten hier wat van de pijn.

Laten we ook de alternatieven niet vergeten - bibliotheken zoals Boost, of het omgaan met UTF-8 met `std::string_view` voor zero-copy operaties op modern C++.

## Zie Ook
- C++ referentie voor `std::string`: https://cplusplus.com/reference/string/string/
- C++ Werkontwerp, Standaard voor Programmeertaal C++: http://www.open-std.org/JTC1/SC22/WG21/docs/papers/2020/n4861.pdf
- Leer meer over `std::format`: https://en.cppreference.com/w/cpp/utility/format
- Boost-bibliotheekdocumentatie: https://www.boost.org/doc/libs/1_75_0/libs/string_algo/doc/html/index.html
