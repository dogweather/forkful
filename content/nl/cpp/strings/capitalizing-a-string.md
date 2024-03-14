---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:55:28.925104-07:00
description: "Het kapitaliseren van een tekenreeks betekent het omzetten van alle\
  \ tekens in de tekst naar hoofdletters. Programmeurs doen dit voor uniformiteit,\
  \ nadruk,\u2026"
lastmod: '2024-03-13T22:44:51.096012-06:00'
model: gpt-4-0125-preview
summary: "Het kapitaliseren van een tekenreeks betekent het omzetten van alle tekens\
  \ in de tekst naar hoofdletters. Programmeurs doen dit voor uniformiteit, nadruk,\u2026"
title: Een string met hoofdletters maken
---

{{< edit_this_page >}}

## Wat & Waarom?
Het kapitaliseren van een tekenreeks betekent het omzetten van alle tekens in de tekst naar hoofdletters. Programmeurs doen dit voor uniformiteit, nadruk, of soms om aan bepaalde gegevensnormen te voldoen.

## Hoe:
C++ biedt verschillende manieren om een string te kapitaliseren, maar hier is een eenvoudige voorbeeld:

```cpp
#include <iostream>
#include <algorithm>
#include <string>

std::string capitalizeString(std::string str) {
    std::transform(str.begin(), str.end(), str.begin(), ::toupper);
    return str;
}

int main() {
    std::string text = "Hallo, Wereld!";
    std::string capitalizedText = capitalizeString(text);
    std::cout << capitalizedText << std::endl;
    return 0;
}
```

Voorbeelduitvoer:
```
HALLO, WERELD!
```

## Diepgaande Duik
Om strings in C++ te kapitaliseren, vertrouwden we historisch gezien op een lus om door elk karakter te itereren, waarbij de `toupper` functie uit `<cctype>` werd toegepast.

Naarmate C++ evolueerde, bood de Standard Template Library (STL) algoritmes zoals `std::transform`, die een functie over een reeks kunnen toepassen. Deze stijl bevordert schonere code en potentieel betere prestaties door algoritmische optimalisaties.

Buiten `std::transform`, is er ook de optie om reeksen te gebruiken (vanaf C++20) wat de code nog beknopter en expressiever maakt. Maar dat is een onderwerp voor een andere dag.

Alternatieven voor het kapitaliseren van strings omvatten het schrijven van je eigen functie of het gebruik van externe bibliotheken zoals Boost. Het komt echt neer op hoeveel controle je nodig hebt en welke afhankelijkheden je wilt aangaan.

Als je `std::transform` gebruikt, wees dan bewust dat het de string direct wijzigt. Als het behouden van de oorspronkelijke tekenreeks belangrijk is, werk dan altijd op een kopie.

## Zie Ook
- C++ Referentie voor `std::transform`: https://en.cppreference.com/w/cpp/algorithm/transform
- C++ Referentie voor `toupper`: https://en.cppreference.com/w/cpp/string/byte/toupper
- Een overzicht van C++20 Reeksen: https://en.cppreference.com/w/cpp/ranges
