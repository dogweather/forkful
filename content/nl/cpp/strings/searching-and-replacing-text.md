---
title:                "Tekst zoeken en vervangen"
aliases: - /nl/cpp/searching-and-replacing-text.md
date:                  2024-01-28T22:06:59.966260-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tekst zoeken en vervangen"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/cpp/searching-and-replacing-text.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Zoeken en vervangen van tekst is het vinden van specifieke strings binnen een grotere string en deze omwisselen voor iets anders. Programmeurs gebruiken dit voor taken zoals het bijwerken van variabelenamen, het wijzigen van gegevens of het automatiseren van bewerkingen in meerdere bestanden.

## Hoe:
C++ biedt verschillende manieren om tekst te zoeken en te vervangen. Hieronder is een voorbeeld met `std::string::find` en `std::string::replace`.

```cpp
#include <iostream>
#include <string>

int main() {
    std::string myText = "The quick brown fox jumps over the lazy dog.";
    std::string wordToSearch = "lazy";
    std::string replacement = "energic";

    size_t pos = myText.find(wordToSearch);
    
    if (pos != std::string::npos) {
        myText.replace(pos, wordToSearch.length(), replacement);
    }

    std::cout << myText << std::endl; // Uitvoer: The quick brown fox jumps over the energic dog.
    return 0;
}
```

## Diepgaande duik
De `find` en `replace` functies maken al eeuwen deel uit van C++'s `std::string` klasse, waardoor ze een eenvoudige maar krachtige manier zijn om tekst te manipuleren. Voordat `std::string` bestond, gebruikten C-programmeurs tekenreeksen en functies zoals `strstr` en `strcpy` uit de C Standaardbibliotheek voor vergelijkbare taken, wat foutgevoeliger was en handmatig geheugenbeheer vereiste.

Wat betreft alternatieven bieden andere componenten van de standaard bibliotheek zoals `std::regex` mogelijkheden voor patroongebaseerde tekstmanipulatie voor complexe zoek- en vervangscenario's. Bibliotheken van derden, zoals Boost, bieden nog geavanceerdere opties voor tekstverwerking.

Intern omvat zoeken en vervangen algoritmes die itereren over een string om overeenkomende sequenties van karakters te vinden en vervolgens de inhoud van de string dienovereenkomstig aan te passen. De efficiëntie van deze bewerkingen kan variëren, afhankelijk van de implementatie en de complexiteit van het zoekpatroon.

## Zie ook
- C++ Referentie voor `std::string::find`: https://en.cppreference.com/w/cpp/string/basic_string/find
- C++ Referentie voor `std::string::replace`: https://en.cppreference.com/w/cpp/string/basic_string/replace
- C++ Referentie voor Reguliere Expressies `std::regex`: https://en.cppreference.com/w/cpp/regex
- Boost String Algorithms Bibliotheek: https://www.boost.org/doc/libs/release/libs/algorithm/string/
