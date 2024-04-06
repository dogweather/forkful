---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:53.438951-07:00
description: 'Hoe: Als `voorbeeld.txt` bevat.'
lastmod: '2024-04-05T21:53:51.144613-06:00'
model: gpt-4-0125-preview
summary: Als `voorbeeld.txt` bevat.
title: Een tekstbestand lezen
weight: 22
---

## Hoe:
```C++
#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::ifstream file("voorbeeld.txt");
    std::string lijn;

    if (file.is_open()) {
        while (getline(file, lijn)) {
            std::cout << lijn << '\n';
        }
        file.close();
    } else {
        std::cout << "Kan bestand niet openen";
    }
    
    return 0;
}
```
Als `voorbeeld.txt` bevat:
```
Hallo, wereld!
Dit is een testbestand.
```
Zal de output zijn:
```
Hallo, wereld!
Dit is een testbestand.
```

## Diepgaande Duik
Vroeger was gegevensopslag en -terugwinning behoorlijk omslachtig. Met de komst van hogere programmeertalen werden operaties zoals het lezen uit een tekstbestand eenvoudiger. C++ biedt verschillende manieren om uit bestanden te lezen, door gebruik te maken van input/output streams die door de standaardbibliotheek worden aangeboden.

Alternatieven voor <fstream> voor bestands-I/O omvatten het gebruik van oudere C-functies (zoals fopen, fgets, etc.), besturingssysteem-specifieke API's, of andere bibliotheken die enkele van de details op een lager niveau wegnemen.

Wanneer we het hebben over implementatiedetails, is het essentieel om te weten dat `std::ifstream` een klasse is die inputbestandstreams beheert. De sleutelfuncties betrokken zijn `is_open()` om te controleren of de bestandstream succesvol is geopend, `getline()` om het bestand regel voor regel te lezen, en `close()` om de bestandstream te sluiten. Het is cruciaal om de bronnen van bestanden correct te beheren om lekken of gegevenscorruptie te voorkomen. Gelukkig omvat modern C++ (C++11 en later) functies zoals RAII, die het beheer van bronnen veiliger kunnen afhandelen via objectlevensduur.

## Zie Ook
- [cppreference.com - Input/output bibliotheek](https://en.cppreference.com/w/cpp/io)
- Stack Overflow: [Hoe kan ik CSV-bestanden lezen en verwerken in C++?](https://stackoverflow.com/questions/1120140/how-can-i-read-and-parse-csv-files-in-c)
