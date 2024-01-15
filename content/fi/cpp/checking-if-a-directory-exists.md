---
title:                "Tarkasta kansio on olemassa"
html_title:           "C++: Tarkasta kansio on olemassa"
simple_title:         "Tarkasta kansio on olemassa"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi?

Joskus ohjelman suorittaminen vaatii tarkistamaan, onko tietyssä sijainnissa oleva kansio olemassa. Tämä voi olla hyödyllistä esimerkiksi tietyn toiminnon suorittamisen ehtona tai tietojen tallentamisen paikkana.

## Miten?

Tarkistaaksesi, onko kansio olemassa, voit käyttää `std::filesystem::exists`-funktiota ja antaa sille kansion absoluuttisen polun parametrina. Esimerkiksi, jos haluat tarkistaa, onko kansio nimeltä "kansio1" sijainnissa "c:\users\käyttäjä", voit käyttää seuraavaa koodia:

```C++
#include <iostream>
#include <filesystem>

int main() {
    std::string kansio = "c:/users/käyttäjä/kansio1";
    if (std::filesystem::exists(kansio)) {
        std::cout << "Kansio on olemassa!" << std::endl;
    }
    else {
        std::cout << "Kansiota ei löydy." << std::endl;
    }
    return 0;
}
```

Tulostus:

```
Kansio on olemassa!
```

## Syvemmälle

`std::filesystem::exists`-funktio käyttää taustalla käyttöjärjestelmän rajapintaa tarkistamaan, onko kansio olemassa. Jos kansio on tarkeentinäytön tallennuspaikassa, tämä funktio voi olla tehokas tapa tarkistaa sen olemassaolo. On kuitenkin tärkeää huomata, että tämä funktio ei anna tarkkaa tietoa siitä, miksi kansioa ei löydy. Se voi olla joko tiedostojärjestelmän tai käyttöoikeuksien rajoitusten takana.

## Katso myös

- [`std::filesystem::exists`-dokumentaatio (en englanniksi)](https://en.cppreference.com/w/cpp/filesystem/exists)
- [Kansiorakenteen tarkistamisen opas (en englanniksi)](https://www.geeksforgeeks.org/check-if-a-directory-exists-in-a-given-path-in-c-c/)