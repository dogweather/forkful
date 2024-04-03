---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:59.966260-07:00
description: 'Hoe: C++ biedt verschillende manieren om tekst te zoeken en te vervangen.
  Hieronder is een voorbeeld met `std::string::find` en `std::string::replace`.'
lastmod: '2024-03-13T22:44:51.097911-06:00'
model: gpt-4-0125-preview
summary: C++ biedt verschillende manieren om tekst te zoeken en te vervangen.
title: Tekst zoeken en vervangen
weight: 10
---

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
