---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:11.786902-07:00
description: "Merkkijonon alkukirjaimen suurentaminen tarkoittaa merkkijonon jokaisen\
  \ sanan ensimm\xE4isen merkin muuttamista suuraakkoseksi, jos se on pienaakkosena,\u2026"
lastmod: 2024-02-19 22:05:15.743942
model: gpt-4-0125-preview
summary: "Merkkijonon alkukirjaimen suurentaminen tarkoittaa merkkijonon jokaisen\
  \ sanan ensimm\xE4isen merkin muuttamista suuraakkoseksi, jos se on pienaakkosena,\u2026"
title: Merkkijonon muuttaminen isoiksi kirjaimiksi
---

{{< edit_this_page >}}

## Mikä & Miksi?
Merkkijonon alkukirjaimen suurentaminen tarkoittaa merkkijonon jokaisen sanan ensimmäisen merkin muuttamista suuraakkoseksi, jos se on pienaakkosena, samalla kun kaikki muut merkit jätetään muuttumattomiksi. Ohjelmoijat suorittavat usein tämän tehtävän tulosteiden muotoilun, käyttäjän syötteiden tai datan käsittelyn yhteydessä varmistaakseen tekstin esitys- tai käsittelytavan johdonmukaisuuden, erityisesti käyttöliittymissä tai datan normalisointitehtävissä.

## Kuinka:
C++:ssa voit suurentaa merkkijonon alkukirjaimen käyttäen standardikirjastoa ilman kolmannen osapuolen kirjastoja. Kuitenkin monimutkaisempia tai erityisempiä suurentamiskäyttäytymisiä varten kirjastot, kuten Boost, voivat olla hyvin hyödyllisiä. Alla on esimerkkejä molemmista lähestymistavoista.

### Käyttäen Standard C++ Kirjastoa:

```cpp
#include <iostream>
#include <cctype> // std::tolower ja std::toupper käyttöön
#include <string>

std::string capitalizeString(const std::string& input) {
    std::string result;
    bool capitalizeNext = true;

    for (char ch : input) {
        if (std::isspace(ch)) {
            capitalizeNext = true;
        } else if (capitalizeNext) {
            ch = std::toupper(ch);
            capitalizeNext = false;
        }
        result += ch;
    }

    return result;
}

int main() {
    std::string text = "hello world from c++";
    std::string capitalizedText = capitalizeString(text);
    std::cout << capitalizedText << std::endl; // Tuloste: "Hello World From C++"
}
```

### Käyttäen Boost Kirjastoa:

Monimutkaisempia merkkijonomanipulaatioita varten, mukaan lukien paikallisesta ympäristöstä tietoinen suurentaminen, saatat haluta käyttää Boost String Algo -kirjastoa.

Varmista ensin, että sinulla on Boost-kirjasto asennettuna ja määritettynä projektissasi. Sen jälkeen voit sisällyttää tarvittavat otsikot ja käyttää sen ominaisuuksia kuten alla on esitetty.

```cpp
#include <boost/algorithm/string.hpp>
#include <iostream>
#include <string>

int main() {
    std::string text = "hello world from c++";
    std::string capitalizedText = text;

    // suurentaa jokaisen sanan ensimmäisen kirjaimen
    boost::algorithm::to_lower(capitalizedText); // varmistetaan, että merkkijono on pienaakkosissa
    capitalizedText[0] = std::toupper(capitalizedText[0]); // suurenna ensimmäinen merkki

    for (std::size_t i = 1; i < capitalizedText.length(); ++i) {
        if (isspace(capitalizedText[i - 1])) { // suurenna välilyönnin jälkeen
            capitalizedText[i] = std::toupper(capitalizedText[i]);
        }
    }

    std::cout << capitalizedText << std::endl; // Tuloste: "Hello World From C++"
}
```

Tässä tapauksessa Boost yksinkertaistaa joitakin merkkijonomanipulaation tehtäviä, mutta edellyttää edelleen räätälöityä lähestymistapaa todelliseen suurentamiseen, koska se tarjoaa pääasiassa muunnos- ja kirjainkoon muutostyökaluja.
