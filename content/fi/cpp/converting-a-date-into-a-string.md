---
title:                "C++: Päivämäärän muuttaminen merkkijonoksi"
simple_title:         "Päivämäärän muuttaminen merkkijonoksi"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi: Converting päivämäärä merkkijonoksi 

On monia tilanteita, jolloin tarvitsemme muuntaa päivämäärä muotoon, joka on helpompi käsitellä, esimerkiksi tallentamaan tietokantaan tai tulostamaan käyttäjälle. Tämä artikkeli käsittelee, miten voimme muuntaa päivämäärän merkkijonoksi käyttämällä C++ -kieltä.

## Kuinka: 


```C++
#include <iostream>
#include <sstream> //sisällytetään stringstream
#include <iomanip> //sisällytetään std::put_time

int main() {
  //luodaan päivämäärä
  std::tm date = {0, 0, 0, //tunti, minuutti, sekunti
                  31, 9, 2020 - 1900 //päivä, kuukausi, vuosi - 1900
  };

  //luodaan stringstream ja asetetaan siihen päivämäärän muoto
  std::stringstream ss;
  ss << std::put_time(&date, "%d.%m.%Y");

  //tulostetaan muunnettu päivämäärä merkkijonona
  std::cout << ss.str();

  return 0;
}

//tulostus: 31.10.2020
```

## Syvällisempi tarkastelu:

Yllä oleva koodiesimerkki käyttää C++ -kielen stringstreamia ja put_time -funktiota päivämäärän muuntamiseen merkkijonoksi. Put_time vaatii kaksi parametria: osoitin tm-tyypin rakenteeseen, joka sisältää päivämäärätiedot, ja merkkijonon muodon, johon päivämäärä muunnetaan.

Esimerkissä luomme ensin tm-rakenteen ja annamme sille päivämäärän tiedot. Sitten luodaan stringstream ja asetetaan siihen muoto, johon päivämäärä halutaan muuttaa. Lopuksi tulostetaan stringstreamin sisältö.

On tärkeää huomata, että päivämäärän muotona käytämme "%d.%m.%Y", mikä vastaa päivä.kuukausi.vuosi -muotoa. Muotoilumerkit vaihtelevat käyttämämme päivämäärän muodon mukaan, joten on tärkeää tarkistaa dokumentaatiosta oikeat merkit.

## Katso myös:

- [C++ reference - std::put_time](https://en.cppreference.com/w/cpp/io/manip/put_time)
- [C++ reference - std::tm](https://en.cppreference.com/w/cpp/chrono/c/tm)