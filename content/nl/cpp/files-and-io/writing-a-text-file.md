---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:16.981770-07:00
description: "Een tekstbestand schrijven in C++ betekent het cre\xEBren of wijzigen\
  \ van een bestand om tekstuele gegevens op te slaan. Programmeurs doen dit om gegevens\u2026"
lastmod: '2024-03-13T22:44:51.131571-06:00'
model: gpt-4-0125-preview
summary: "Een tekstbestand schrijven in C++ betekent het cre\xEBren of wijzigen van\
  \ een bestand om tekstuele gegevens op te slaan. Programmeurs doen dit om gegevens\u2026"
title: Een tekstbestand schrijven
weight: 24
---

## Wat & Waarom?
Een tekstbestand schrijven in C++ betekent het creëren of wijzigen van een bestand om tekstuele gegevens op te slaan. Programmeurs doen dit om gegevens zoals configuraties, logs of door gebruikers gegenereerde inhoud te bewaren.

## Hoe:
Hieronder staat een eenvoudig C++ programma dat een tekstbestand creëert en er "Hallo, Wereld!" naar schrijft.

```c++
#include <fstream>
#include <iostream>

int main() {
    std::ofstream outfile("hallo.txt");

    if (outfile.is_open()) {
        outfile << "Hallo, Wereld!";
        outfile.close();
        std::cout << "Bestand succesvol geschreven\n";
    } else {
        std::cout << "Fout bij het openen van bestand\n";
    }

    return 0;
}
```
Voorbeelduitvoer:
```
Bestand succesvol geschreven
```

## Diepere Duik
In C++ worden bestanden beheerd door de header `<fstream>`, die `std::ofstream` biedt voor het schrijven, `std::ifstream` voor het lezen, en `std::fstream` voor beide. Historisch gezien, is file I/O in C++ geëvolueerd van de C `FILE` structuur en gerelateerde functies. Alternatieven voor `fstream` omvatten platform-specifieke API's, bibliotheken van derden, of moderne C++ voorstellen zoals uitbreidingen van de bestandssysteembibliotheek. Bij het schrijven van bestanden, handel fouten af en zorg ervoor dat bronnen goed worden vrijgegeven, doorgaans met behulp van RAII patronen beschikbaar in modern C++.

## Zie Ook
- C++ Bestand I/O: http://www.cplusplus.com/doc/tutorial/files/
- C++ Referentie (ofstream): https://en.cppreference.com/w/cpp/io/basic_ofstream
- C++ Bestandssysteembibliotheek: https://en.cppreference.com/w/cpp/filesystem
