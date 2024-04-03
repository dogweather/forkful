---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:22.856308-07:00
description: 'Hoe: Laten we tekens verwijderen met `erase` en `remove_if` samen met
  lambda-expressies. Hier is een snel voorbeeld.'
lastmod: '2024-03-13T22:44:51.096964-06:00'
model: gpt-4-0125-preview
summary: Laten we tekens verwijderen met `erase` en `remove_if` samen met lambda-expressies.
title: Karakters verwijderen die overeenkomen met een patroon
weight: 5
---

## Hoe:
Laten we tekens verwijderen met `erase` en `remove_if` samen met lambda-expressies. Hier is een snel voorbeeld:

```cpp
#include <iostream>
#include <algorithm>
#include <string>

int main() {
    std::string data = "B4n4n4!";

    // Verwijder alle numerieke tekens
    data.erase(std::remove_if(data.begin(), data.end(), ::isdigit), data.end());
    
    std::cout << data << std::endl; // Uitvoer: Bnn!
    
    return 0;
}
```
Voorbeelduitvoer:
```
Bnn!
```

## Diepgang
Het algoritme `std::remove_if` uit de `<algorithm>`-header verkleint de string eigenlijk niet; het herordent elementen en retourneert een pointer naar het nieuwe logische einde. De `erase`-methode van de `std::string`-klasse verwijdert vervolgens het "dode hout" van het einde. Deze combinatie is er sinds C++98 en blijft efficiënt en populair.

Alternatieven? Voor complexe patronen is regex (`<regex>`) je Zwitsers zakmes. Maar, het is overkill voor eenvoudige klusjes.

Details? `std::remove_if` en vergelijkbare algoritmes leunen op iterators, die C++ heeft overgenomen van de Standard Template Library (STL) midden jaren 90. Ze bevorderen generiek programmeren, waardoor je knip-en-verander-code werkt op strings, lijsten, noem maar op.

## Zie Ook
- C++ referentie voor `std::remove_if`: https://en.cppreference.com/w/cpp/algorithm/remove
- C++ referentie voor `std::string::erase`: https://en.cppreference.com/w/cpp/string/basic_string/erase
- Meer over iterators in C++: https://en.cppreference.com/w/cpp/iterator
- Wanneer `std::regex` te gebruiken voor patroonherkenning: https://en.cppreference.com/w/cpp/regex
