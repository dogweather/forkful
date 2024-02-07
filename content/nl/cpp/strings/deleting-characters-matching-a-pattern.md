---
title:                "Karakters verwijderen die overeenkomen met een patroon"
date:                  2024-01-28T21:58:22.856308-07:00
model:                 gpt-4-0125-preview
simple_title:         "Karakters verwijderen die overeenkomen met een patroon"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/cpp/deleting-characters-matching-a-pattern.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Het verwijderen van tekens die overeenkomen met een patroon betekent het uitsluiten van specifieke sequenties uit een string. Programmeurs doen dit voor opschoning, gegevensopmaak, of om aan applicatieregels te voldoen.

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
