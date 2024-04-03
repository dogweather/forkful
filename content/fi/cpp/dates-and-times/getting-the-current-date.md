---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:07.425788-07:00
description: "Miten: C++ tarjoaa useita tapoja saada nykyinen p\xE4iv\xE4m\xE4\xE4\
  r\xE4, mukaan lukien C++ standardikirjasto ja kolmannen osapuolen kirjastot kuten\
  \ Boost. Seuraavat\u2026"
lastmod: '2024-03-13T22:44:56.875682-06:00'
model: gpt-4-0125-preview
summary: "C++ tarjoaa useita tapoja saada nykyinen p\xE4iv\xE4m\xE4\xE4r\xE4, mukaan\
  \ lukien C++ standardikirjasto ja kolmannen osapuolen kirjastot kuten Boost."
title: "Nykyisen p\xE4iv\xE4m\xE4\xE4r\xE4n hankkiminen"
weight: 29
---

## Miten:
C++ tarjoaa useita tapoja saada nykyinen päivämäärä, mukaan lukien C++ standardikirjasto ja kolmannen osapuolen kirjastot kuten Boost. Seuraavat esimerkit osoittavat, miten tämä tehtävä suoritetaan.

### Käyttäen `<chrono>`-kirjastoa (C++20 ja myöhemmät)
C++20 toi `<chrono>`-kirjastoon lisää toiminnallisuuksia, mikä tekee nykyisen päivämäärän saamisen suoraviivaiseksi:
```cpp
#include <iostream>
#include <chrono>
#include <format> // For std::format (C++20)

int main() {
    auto current_time_point = std::chrono::system_clock::now(); // Otetaan kiinni nykyinen aika
    auto current_time_t = std::chrono::system_clock::to_time_t(current_time_point); // Muunnetaan time_t:ksi

    // Muotoillaan aika luettavaan muotoon
    std::cout << "Nykyinen päivämäärä: " << std::format("{:%Y-%m-%d}", std::chrono::system_clock::to_time_t(current_time_point)) << std::endl;

    return 0;
}
```
**Esimerkkituloste:**
```plaintext
Nykyinen päivämäärä: 2023-03-15
```

### Käyttäen `<ctime>`-kirjastoa
Ohjelmoijille, jotka työskentelevät vanhempien C++ versioiden kanssa tai jotka suosivat perinteistä C-kirjastoa:
```cpp
#include <iostream>
#include <ctime>

int main() {
    std::time_t t = std::time(0); // Haetaan nykyinen aika
    std::tm* now = std::localtime(&t);
    std::cout << "Nykyinen päivämäärä: " 
              << (now->tm_year + 1900) << '-' 
              << (now->tm_mon + 1) << '-'
              <<  now->tm_mday
              << std::endl;

    return 0;
}
```
**Esimerkkituloste:**
```plaintext
Nykyinen päivämäärä: 2023-03-15
```

### Käyttäen Boost Date_Time -kirjastoa
Projekteille, jotka hyödyntävät Boost-kirjastoja, Boost Date_Time -kirjasto tarjoaa vaihtoehtoisen menetelmän nykyisen päivämäärän saamiseen:
```cpp
#include <iostream>
#include <boost/date_time.hpp>

int main() {
    // Haetaan nykyinen päivä käyttäen Boostin gregoriaanista kalenteria
    boost::gregorian::date today = boost::gregorian::day_clock::local_day();
    std::cout << "Nykyinen päivämäärä: " << today << std::endl;

    return 0;
}
```
**Esimerkkituloste:**
```plaintext
Nykyinen päivämäärä: 2023-Mar-15
```
Nämä esimerkit tarjoavat perustan työskentelyyn päivämäärien kanssa C++:ssa, mikä on olennaista laajalle valikoimalle sovelluksia.
