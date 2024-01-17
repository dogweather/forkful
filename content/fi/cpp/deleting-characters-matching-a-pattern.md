---
title:                "Pohjautuvien merkkien poisto"
html_title:           "C++: Pohjautuvien merkkien poisto"
simple_title:         "Pohjautuvien merkkien poisto"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Mikä on merkkijonon osien poistaminen ja miksi ohjelmoijat tekevät sitä? Merkkijonon osien poistaminen on prosessi, jossa ohjelma etsii ja poistaa tietyn kaavan mukaiset merkit merkkijonosta. Tätä voidaan käyttää esimerkiksi turhien välilyöntien, numero- tai erikoismerkkien poistamiseen merkkijonosta. Ohjelmoijat tekevät tätä parantaakseen ohjelmien suorituskykyä ja helpottaakseen käsiteltävien tietojen käsittelyä.

## Miten tehdään:
```
#include <iostream>
#include <string>

using namespace std;

int main() {
    //Alustetaan merkkijono
    string teksti = " Tämä on esimerkkiteksti! ";
    //Poistetaan välilyönnit merkkijonon alusta ja lopusta
    teksti.erase(0, teksti.find_first_not_of(' '));
    teksti.erase(teksti.find_last_not_of(' ') + 1);
    //Tulostetaan lopputulos
    cout << teksti << "\n";
    return 0;
}
```
**Tulostus:**
```
Tämä on esimerkkiteksti!
```

## Syvempi sukellus:
Merkkijonon osien poistamisella on pitkä historia ohjelmoinnissa. Aikaisemmin tämä tehtiin usein manuaalisesti tai käyttämällä monimutkaisia koodinpätkiä. Nykyään se on kuitenkin yksinkertaistunut ja tehokkaampaa käyttämällä valmiita funktioita, kuten ```erase()```. Toinen vaihtoehto on käyttää säännöllisiä lausekkeita (regular expressions), mutta tämä voi olla monimutkaisempaa ja hitaampaa toteuttaa.

Merkkijonon osien poistamisen toteutus perustuu usein käsittelemään merkkijonoa merkkien ja indeksien avulla. Tästä syystä on tärkeää huolehtia siitä, että indeksit jaettaessa tai lisättäessä ovat oikein, jotta merkkijono pysyy eheänä.

## Katso myös:
- [C++ String Manipulation](https://www.geeksforgeeks.org/string-manipulation-in-c/#erase)
- [Regular Expressions in C++](https://www.geeksforgeeks.org/regular-expressions-in-c-cset-1-introduction/)