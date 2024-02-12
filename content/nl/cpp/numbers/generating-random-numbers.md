---
title:                "Willekeurige getallen genereren"
aliases:
- nl/cpp/generating-random-numbers.md
date:                  2024-01-28T22:00:59.900066-07:00
model:                 gpt-4-0125-preview
simple_title:         "Willekeurige getallen genereren"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/cpp/generating-random-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Het genereren van willekeurige getallen in programmeren houdt in dat er reeksen getallen worden gecreëerd die geen voorspelbare volgorde of patroon hebben. Programmeurs gebruiken deze getallen vaak voor verschillende doeleinden zoals het simuleren van onvoorspelbare gebeurtenissen, in testen en debuggen, en in spelalgoritmen om eerlijkheid of onvoorspelbaarheid te waarborgen.

## Hoe te:

Om willekeurige getallen in C++ te genereren, maak je doorgaans gebruik van de `<random>` header, die werd geïntroduceerd in C++11 en een breed scala aan faciliteiten biedt voor het genereren van willekeurige getallen uit verschillende distributies.

```C++
#include <iostream>
#include <random>

int main() {
    // Initialiseer een willekeurige engine
    std::random_device rd;  
    std::mt19937 gen(rd()); 

    // Definieer het bereik [0, 99] inclusief
    std::uniform_int_distribution<> distrib(0, 99); 

    // Genereer en print 5 willekeurige getallen binnen het gedefinieerde bereik
    for(int n=0; n<5; ++n)
        std::cout << distrib(gen) << ' ';
    return 0;
}
```

Dit codevoorbeeld initialiseert een Mersenne Twister willekeurige getallengenerator met een zaadje van `std::random_device`. Vervolgens definieert het een uniforme integerdistributie in het bereik [0, 99] en print uiteindelijk 5 willekeurige getallen uit deze distributie.

Een voorbeelduitvoer kan er zo uitzien, maar houd er rekening mee dat elke uitvoering waarschijnlijk verschillende resultaten zal produceren:

```
45 67 32 23 88
```

## Diepere Duik:

Historisch gezien leunde de generatie van willekeurige getallen in C++ sterk op de `rand()` functie en de `srand()` functie voor het zaaien, gevonden in de `<cstdlib>` header. Deze benadering kreeg echter vaak kritiek vanwege het gebrek aan uniformiteit en voorspelbaarheid in de distributie van gegenereerde getallen.

De introductie van de `<random>` header in C++11 markeerde een aanzienlijke verbetering en bood een geavanceerd systeem voor het produceren van willekeurige getallen. De geboden faciliteiten omvatten een verscheidenheid aan motoren (zoals `std::mt19937` voor Mersenne Twister) en distributies (zoals `std::uniform_int_distribution` voor uniforme distributie van integers) die gecombineerd kunnen worden om aan de specifieke behoeften van de programmeur te voldoen, wat leidt tot voorspelbaarder gedrag, betere prestaties en grotere flexibiliteit.

Hoewel de `<random>` bibliotheek veel beter is dan de oudere `rand()` benadering, is het de moeite waard om op te merken dat het genereren van echt willekeurige getallen - vooral voor cryptografische doeleinden - nog steeds afhankelijk is van aanvullende overwegingen. Voor cryptografische toepassingen moeten bibliotheken die specifiek zijn ontworpen voor beveiliging, die vaak hardware-entropiebronnen gebruiken, in plaats daarvan worden gebruikt.
