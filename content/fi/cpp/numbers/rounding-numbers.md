---
date: 2024-01-26 03:43:35.444701-07:00
description: "Kuinka: C++ tarjoaa useita tapoja py\xF6rist\xE4\xE4 numeroita, kuten\
  \ `floor()`, `ceil()` ja `round()`."
lastmod: '2024-03-13T22:44:56.860064-06:00'
model: gpt-4-0125-preview
summary: "C++ tarjoaa useita tapoja py\xF6rist\xE4\xE4 numeroita, kuten `floor()`,\
  \ `ceil()` ja `round()`."
title: "Numerojen py\xF6rist\xE4minen"
weight: 13
---

## Kuinka:
C++ tarjoaa useita tapoja pyöristää numeroita, kuten `floor()`, `ceil()` ja `round()`:

```C++
#include <iostream>
#include <cmath> // pyöristysfunktioita varten

int main() {
    double num = 3.14;

    std::cout << "floor: " << std::floor(num) << "\n"; // Tuloste: floor: 3
    std::cout << "ceil: " << std::ceil(num) << "\n";   // Tuloste: ceil: 4
    std::cout << "round: " << std::round(num) << "\n"; // Tuloste: round: 3

    // Määritellylle tarkkuudelle pyöristäminen, kuten kahden desimaalin tarkkuuteen:
    double precise_num = 3.146;
    double multiplier = 100.0;
    double rounded = std::round(precise_num * multiplier) / multiplier;

    std::cout << "pyöristettynä kahteen desimaaliin: " << rounded << "\n"; // Tuloste: pyöristettynä kahteen desimaaliin: 3.15

    return 0;
}
```

## Syväsukellus
Ennen C++11-versiota, pyöristämisessä turvauduttiin manuaalisiin tekniikoihin tai ei-standardi kirjastoihin. Nykyään `<cmath>` tarjoaa vankkoja metodeja. `floor()` pyöristää alaspäin, `ceil()` ylöspäin, kun taas `round()` menee lähimpään kokonaislukuun, käsitellen jopa tasapelistilanteet (0.5 tapaukset) pyöristämällä parilliseen numeroon.

Näiden funktioiden käyttäytymisen ymmärtäminen on elintärkeää; esimerkiksi negatiiviset numerot voivat aiheuttaa sekaannusta (`std::round(-2.5)` tuottaa `-2.0`).

Vaihtoehtoja? Casting intiksi 0.5 lisäämisen jälkeen positiivisille numeroille oli perinteinen kikka, mutta se epäonnistuu negatiivisilla ja ei ole tyypistä riippumaton. Kirjastot, kuten Boost, voivat tarjota hienovaraisempia lähestymistapoja, kun taas kielilisäykset tai kääntäjän sisäiset funktiot voivat optimoida tiettyä laitteistoa varten.

## Katso Myös
- C++ viite `<cmath>`:lle: https://en.cppreference.com/w/cpp/header/cmath
- IEEE-standardi liukulukuaritmetiikalle (IEEE 754): https://ieeexplore.ieee.org/document/4610935
- Boost Numerinen Muunnoskirjasto: https://www.boost.org/doc/libs/release/libs/numeric/conversion/
