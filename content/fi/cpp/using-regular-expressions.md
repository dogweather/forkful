---
title:                "Säännöllisten lausekkeiden käyttö"
html_title:           "C++: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi

Regular expressiont ovat tehokas työkalu tekstin jäsentämiseen ja muokkaamiseen. Niiden avulla voidaan helposti etsiä, korvata ja poistaa tiettyjä merkkijonoja, mikä säästää aikaa ja vaivaa manuaaliselta käsittelyltä.

## Miten

Regular expressionit koostuvat erilaisista säännöistä, joiden avulla määritetään, mitä tiettyä merkkijonoa halutaan etsiä tai muokata. Tässä esimerkissä käytetään `std::regex` -kirjastoa, joka tarjoaa valmiita funktioita regular expressionien käyttämiseen.

```C++
// Alustetaan merkkijono, johon regular expressionia käytetään
std::string teksti = "Tämä on esimerkki kodin osoitteesta: Käytäväkatu 5";

// Luodaan sääntö, jolla haetaan kaikki numerot merkkijonosta
std::regex sääntö("\\d+");

// Alustetaan muuttuja löydetyille merkkijonoille
std::smatch osumat;

// Etsitään merkkijonosta kaikki numerot säännön avulla
while (std::regex_search(teksti, osumat, sääntö)) {
    // Tulostetaan löydetty merkkijono
    std::cout << osumat.str() << std::endl;

    // Siirrytään seuraavaan löydettyyn kohtaan merkkijonossa
    teksti = osumat.suffix().str();
}
```

Tulostaa:

```
5
```

## Syventävä tieto

Regular expressionit käyttävät erilaisia merkkejä ja sääntöjä, joilla voidaan tarkentaa haettavaa tai muokattavaa merkkijonoa. Tässä muutamia esimerkkejä yleisesti käytetyistä merkeistä ja säännöistä:

- `.` - Tarkoittaa minkä tahansa merkin paitsi rivin vaihtumisen.
- `?` - Tarkoittaa, että edellinen merkki tai sääntö voi esiintyä nolla tai yhden kerran.
- `*` - Tarkoittaa, että edellinen merkki tai sääntö voi esiintyä nolla tai useamman kerran.
- `+` - Tarkoittaa, että edellinen merkki tai sääntö esiintyy vähintään yhden kerran.
- `()` - Luo ryhmän, jolla voi olla omia erityisiä sääntöjä.

Regular expressionien käyttö vaatii hieman totuttelua, mutta niiden avulla on mahdollista tehdä erittäin monimutkaisiakin hakuja ja muokkauksia teksteihin.

## Katso myös

- [C++ std::regex -dokumentaatio](https://en.cppreference.com/w/cpp/regex)
- [Regular expression cheat sheet](https://www.debuggex.com/cheatsheet/regex/cpp)