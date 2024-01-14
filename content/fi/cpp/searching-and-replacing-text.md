---
title:                "C++: Tekstin etsiminen ja korvaaminen"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi?

Tekstin etsiminen ja korvaaminen on tärkeä osa ohjelmoinnin prosessia, sillä se auttaa meitä muokkaamaan ja parantamaan koodia helposti ja nopeasti. Tekstin etsiminen ja korvaaminen on myös erittäin hyödyllistä, kun haluamme tehdä laajoja muutoksia koodiin, ilman että joudumme tekemään niitä manuaalisesti.

## Miten?

Tekstin etsiminen ja korvaaminen on helppoa ja vaivatonta C++-ohjelmointikielellä. Voimme käyttää ```find()``` ja ```replace()``` funktioita, jotka ovat osa STL-kirjastoa.

Esimerkiksi, jos haluamme etsiä ja korvata kaikki "hello" -merkit "hei", voimme tehdä sen seuraavasti:

```
#include <iostream>
#include <string>

using namespace std;

int main() {
    string teksti = "hello world!";
    cout << teksti << endl;
    
    // Etsii ja korvaa merkkijonon "hello" merkkijonolla "hei"
    teksti.replace(teksti.find("hello"), 5, "hei");
    
    cout << teksti << endl;
    
    return 0;
}
```

Tämän koodin ulostulo olisi: "hei world!". Huomaathan, että ```replace()``` funktiossa on kolme parametria: ensimmäinen on haettavan merkkijonon alkuindeksi, toinen on korvattavan merkkijonon pituus ja kolmas on korvaava merkkijono.

Voimme myös käyttää ```replace()``` funktiota poistaaksemme merkkijonon, jolloin korvaava merkkijono olisi tyhjä.

## Syväsukellus

Tekstin etsiminen ja korvaaminen on erittäin hyödyllistä, kun työskentelemme isojen tekstimassojen kanssa, kuten tiedostojen käsittelyssä. Voimme myös käyttää säännöllisiä lausekkeita tekstien etsimiseen ja korvaamiseen, mikä tekee prosessista vielä tehokkaamman.

Säännölliset lausekkeet ovat merkkijonoja, jotka kuvaavat tiettyjä haku- tai korvaussääntöjä. Voimme käyttää niitä laajempien, monimutkaisempien merkkijonojen etsimiseen ja korvaamiseen.

## Katso myös

- [C++ Standard Library (CPPreference)](https://en.cppreference.com/w/cpp/string/basic_string/replace)
- [Säännöllisten lausekkeiden opas (W3Schools)](https://www.w3schools.com/cpp/cpp_regex.asp)
- [Säännöllisten lausekkeiden käyttö C++:ssa (GeeksforGeeks)](https://www.geeksforgeeks.org/regular-expression-in-c-c/)

Kiitos lukemisesta! Toivottavasti tämä artikkeli auttaa sinua tekstin etsimisessä ja korvaamisessa C++:ssa. Muista kokeilla erilaisia käyttötapoja ja pääset varmasti alkuun nopeasti. Onnea koodaamiseen!