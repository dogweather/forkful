---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:13.799318-07:00
description: "Hoe te: In C++ komen associatieve arrays tot leven met de `<map>` en\
  \ `<unordered_map>` headers. Laten we enkele voorbeelden doorlopen om beide in actie\
  \ te\u2026"
lastmod: '2024-03-13T22:44:51.105544-06:00'
model: gpt-4-0125-preview
summary: In C++ komen associatieve arrays tot leven met de `<map>` en `<unordered_map>`
  headers.
title: Gebruik van associatieve arrays
weight: 15
---

## Hoe te:
In C++ komen associatieve arrays tot leven met de `<map>` en `<unordered_map>` headers. Laten we enkele voorbeelden doorlopen om beide in actie te zien.

### Gebruikmakend van `std::map`
`std::map` houdt elementen gesorteerd op basis van de sleutel. Hier is hoe je begint:

```C++
#include <iostream>
#include <map>
#include <string>

int main() {
    std::map<std::string, int> ageMap;
    
    // Waarden invoegen
    ageMap["Alice"] = 30;
    ageMap["Bob"] = 25;
    
    // Waarden toegang
    std::cout << "Bob's leeftijd: " << ageMap["Bob"] << std::endl;
    
    // Itereren over een map
    for(const auto &paar : ageMap) {
        std::cout << paar.first << " is " << paar.second << " jaar oud." << std::endl;
    }
    
    return 0;
}
```

### Gebruikmakend van `std::unordered_map`
Wanneer volgorde er niet toe doet, maar prestatie wel, is `std::unordered_map` je vriend, met een snellere gemiddelde complexiteit voor invoegingen, opzoekingen en verwijderingen.

```C++
#include <iostream>
#include <unordered_map>
#include <string>

int main() {
    std::unordered_map<std::string, double> productPrijs;
    
    // Waarden invoegen
    productPrijs["melk"] = 2.99;
    productPrijs["brood"] = 1.99;
    
    // Waarden toegang
    std::cout << "Melkprijs: $" << productPrijs["melk"] << std::endl;
    
    // Itereren over een unordered_map
    for(const auto &paar : productPrijs) {
        std::cout << paar.first << " kost $" << paar.second << std::endl;
    }
    
    return 0;
}
```

## Diepgaande Verkenning
Associatieve arrays in C++, in het bijzonder `std::map` en `std::unordered_map`, gaan niet alleen over het opslaan van elementen. Ze bieden een fundament voor meer complex beheer van gegevens door operaties zoals zoeken, invoegen en verwijderen mogelijk te maken in efficiënte tijdcomplexiteiten (logaritmisch voor `std::map` en gemiddeld constante tijd voor `std::unordered_map`). Deze efficiëntie komt voort uit de onderliggende datastructuren: een gebalanceerde boom voor `std::map` en een hash-tabel voor `std::unordered_map`.

Historisch gezien, voordat deze deel uitmaakten van de standaardbibliotheek, moesten programmeurs hun eigen versies implementeren of gebruikmaken van bibliotheken van derden, wat leidde tot inconsistenties en mogelijke inefficiënties. De inclusie van maps in C++'s standaardbibliotheek heeft niet alleen hun gebruik gestandaardiseerd maar ook geoptimaliseerd voor prestaties over verschillende compilers en platformen heen.

Hoewel beide krachtig zijn, hangt de keuze tussen een `std::map` en `std::unordered_map` af van de specifieke details van je gebruikssituatie. Heb je geordende gegevens nodig en vind je een kleine prestatievermindering niet erg? Ga dan voor `std::map`. Als je op zoek bent naar snelheid en volgorde je niet uitmaakt, is `std::unordered_map` waarschijnlijk je beste keuze.

Het is echter belangrijk om op te merken dat wanneer je werkt met complexe datastructuren, er altijd compromissen zijn. In sommige nichegevallen kunnen andere datastructuren of zelfs bibliotheken van derden een betere prestatie of functionaliteit bieden die geschikt is voor je specifieke behoeften. Weeg altijd je opties af op basis van de vereisten van je project.
