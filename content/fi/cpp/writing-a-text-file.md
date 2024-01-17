---
title:                "Tekstitiedoston kirjoittaminen"
html_title:           "C++: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

Tekstitiedoston kirjoittaminen tarkoittaa tietojen tallentamista tiedostoon tekstimuodossa, jonka avulla niitä voidaan lukea ja käsitellä myöhemmin ohjelmointikoodissa. Tämä on hyödyllistä esimerkiksi silloin kun halutaan tallentaa käyttäjän antamia tietoja tai tallentaa tuloksia ohjelman suorituksista.

## Miten:

```C++
#include <iostream>
#include <fstream>

int main() {
    std::ofstream tiedosto("tiedosto.txt"); // Avataan uusi tekstitiedosto
    if (tiedosto.is_open()) { // Tarkistetaan, että tiedosto on auki
        tiedosto << "Tallennettava tieto" << std::endl; // Tallennetaan tiedosto teksti
        tiedosto.close(); // Suljetaan tiedosto
        std::cout << "Tiedosto tallennettu!" << std::endl;
    } else {
        std::cout << "Virhe tiedoston avaamisessa!" << std::endl;
    }
    return 0;
}
```

```
Tiedosto tallennettu!
```

## Syventävä sukellus:

Tekstitiedoston kirjoittaminen on ollut olennainen osa ohjelmointia jo pitkään, sillä se mahdollistaa tietojen tallentamisen ja lukemisen helposti ja tehokkaasti. Tekstitiedostoja voivat myös käsitellä eri ohjelmointikielien ohella muutkin ohjelmat, kuten taulukkolaskentaohjelmat.

Kirjoittaessa tekstitiedostoa, on tärkeä tarkastella kirjoittamistilannetta: halutaanko luoda uusi tiedosto vai päivittää jo olemassa olevaa. Varmista myös, että tiedostoon tallennettavat tiedot ovat oikeassa muodossa ja että tiedosto suljetaan oikein, jotta tiedot tallentuvat oikein.

Muita tapoja tallentaa tietoja ovat esimerkiksi käyttämällä tietokantoja tai binääritiedostoja. Nämä tarjoavat usein tehokkaamman ja turvallisemman tavaran tallentaa suuria määriä tietoja, mutta tekstitiedostojen käyttö on silti hyödyllistä esimerkiksi yksinkertaisissa käyttötapauksissa.

## Katso myös:

- [Vuoden 2014 C++-ohjelmointiopas] (http://www.cplusplus.com/doc/tutorial/files/) tarjoaa hyödyllistä tietoa tekstitiedostojen kirjoittamiseen C++:lla.
- [Cppreference-sivusto] (https://en.cppreference.com/w/cpp/), jossa voi löytää tarkempia tietoja C++:sta ja sen käyttämisestä.