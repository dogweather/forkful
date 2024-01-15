---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "C++: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi

Monet ohjelmointityöt, erityisesti monimutkaisemmat projektityöt, vaativat suuren määrän koodin kirjoittamista. Välillä on mahdollista, että kirjoittamisen jälkeen huomataan, että tekstissä on muutettava jotain esimerkiksi korjataksesi virheen tai päivittääksesi tietyn osan koodista. Tässä tapauksessa tekstiä voidaan etsiä ja korvata tietyillä koodinpätkillä, mikä säästää aikaa ja vaivaa.

## Miten

```
#include <iostream>
#include <string>

int main() {
  // Alustetaan merkkijono
  std::string teksti = "Tämä on esimerkkikoodia.";

  // Etsitään ja korvataan tekstistä sana "esimerkki" sanalla "harjoitus"
  teksti.replace(11, 9, "harjoitus");

  // Tulostetaan korjattu teksti
  std::cout << teksti << std::endl;

  return 0;
}
```
Koodissa ensin määritellään tarvittavat kirjastot ja sitten luodaan merkkijono, josta etsitään ja korvataan tekstiä. Käyttämällä "replace" -funktiota, merkitään paikka tekstissä, jossa haluttu osa vaihdetaan sekä korvaavan osan teksti. Lopuksi tulostetaan muokattu teksti. Koodissa voidaan myös käyttää muita toimintoja, kuten "find" ja "find_last_of", jotka löytävät tietystä merkkijonosta tietyn kohdan, jota voidaan käyttää sitten tekstin korvaamisessa. 

## Syvällisempi katsaus

"C++: ssä" on monia muita toimintoja, jotka helpottavat tekstin etsimistä ja korvaamista. Yksi esimerkki on "regex_replace" -funktio, joka käyttää säännöllisiä lausekkeita tekstin muokkaamiseen. Tämä antaa enemmän hallintaa haettavien sanojen suhteen, mikä on erityisen hyödyllistä monimutkaisemmissa projekteissa. Lisäksi nämä toiminnot voidaan yhdistää silmukkaan, jolloin voidaan käydä läpi suuri määrä tekstiä ja korvata tietty sana tai lause jokaisessa esiintymässä.

## Katso myös

- [C++ string replace documentation](https://www.cplusplus.com/reference/string/string/replace/)
- [C++ regex_replace documentation](https://www.cplusplus.com/reference/regex/regex_replace/)