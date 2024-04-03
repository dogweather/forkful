---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:56.160251-07:00
description: "Het verkrijgen van de huidige datum in je C++ programma kan handig zijn:\
  \ denk aan loggen, tijdstempels of planningsfuncties. Het gaat erom relevant te\u2026"
lastmod: '2024-03-13T22:44:51.123169-06:00'
model: gpt-4-0125-preview
summary: 'Het verkrijgen van de huidige datum in je C++ programma kan handig zijn:
  denk aan loggen, tijdstempels of planningsfuncties.'
title: Het huidige datum ophalen
weight: 29
---

## Hoe te:
Hier is hoe je de huidige datum kunt ophalen met `<chrono>`—modern, schoon, zonder nonsens.

```C++
#include <iostream>
#include <chrono>
#include <ctime>

int main() {
    // Huidige systeemtijd ophalen
    auto nu = std::chrono::system_clock::now();

    // Omzetten naar time_t, dan naar tm voor een leesbaar formaat
    std::time_t nu_c = std::chrono::system_clock::to_time_t(nu);
    std::tm* nu_tm = std::localtime(&nu_c);

    // Afdrukken in JJJJ-MM-DD formaat
    std::cout << (nu_tm->tm_year + 1900) << '-' 
              << (nu_tm->tm_mon + 1) << '-'
              <<  nu_tm->tm_mday << '\n';

    return 0;
}
```

Het voorbeeld van de uitvoer die je vandaag zou krijgen:
```
2023-4-14
```
Niet fancy, doet wat het moet doen.

## Diepere Duik
Vroeger waren de C-tijdfuncties de norm—`<ctime>` was je beste keuze. Maar met C++11 en later nam `<chrono>` de schijnwerpers over. Het is typeveilig en vermijdt veelvoorkomende fouten met ouderwetse C-functies.

Alternatieven? Zeker. Je zou oude `std::time` kunnen gebruiken of zelfs OS-specifieke API's als je graag op het randje leeft (of zeer specifieke behoeften hebt).

En implementatiedetails? `<chrono>` vertegenwoordigt tijdspunten, duur en klokken. Het is nauwkeurig en zorgvuldig ontworpen. Tijd is lastig (schrikkelseconden, tijdzones), en `<chrono>` handelt deze complexiteit onder de motorkap af, zodat jij je er geen zorgen over hoeft te maken.

## Zie Ook
- [C++ Referentie - `<chrono>` bibliotheek](https://en.cppreference.com/w/cpp/chrono)
- [C++ Referentie - Ouderwetse `<ctime>`](https://en.cppreference.com/w/cpp/header/ctime)
- Voor een grotere diepe duik, bekijk Howard Hinnant's datumbibliotheek, een uitbreiding op `<chrono>`: [https://github.com/HowardHinnant/date](https://github.com/HowardHinnant/date)
- Als je meteen ondersteuning voor tijdzones nodig hebt, probeer dit: [https://en.cppreference.com/w/cpp/chrono/current_zone](https://en.cppreference.com/w/cpp/chrono/current_zone)
