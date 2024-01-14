---
title:    "C++: Merkkijonon muuttaminen pieniksi kirjaimiksi"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Miksi

Joskus ohjelmointiprojekteissa saattaa olla tarpeen muuttaa merkkijono pieniksi kirjaimiksi. Tässä blogikirjoituksessa käsittelemme, miten tämä onnistuu C++ -ohjelmointikielellä.

## Miten

Merkkijonon muuttaminen pieniksi kirjaimiksi C++:ssa onnistuu käyttämällä `transform`-funktiota ja `tolower`-funktiota. Esimerkiksi, muuttaaksesi merkkijonon `myString` pieniksi kirjaimiksi, sinun tulee käyttää seuraavia rivejä koodissa:

```C++
transform(myString.begin(), myString.end(), myString.begin(), ::tolower);
```

Tässä koodissa `transform`-funktio ottaa parametreinaan merkkijonon alku- ja loppuosoitteet sekä toiminnon, jota halutaan suorittaa jokaiselle merkille. `::tolower` käyttää C-kirjaston `tolower`-funktiota, joka muuttaa annetun merkin pienen kirjaimen ASCII-muotoon.

Kun suoritat tämän koodinpätkän, merkkijonosi muuttuu näin:

**Input:** "TÄMÄ ON ESIMERKKI"

**Output:** "tämä on esimerkki"

## Syvällisempi tarkastelu

Yllä oleva esimerkki oli yksinkertaisimmillaan, mutta on hyvä tietää, että `std::transform` voi myös ottaa kolmannen funktion parametrina. Tämä funktio määrittää, mitkä merkit eivät muutu. Esimerkiksi jos haluat pitää merkkijonossa vain isot kirjaimet, voit käyttää funktiota `::isupper`, joka tarkistaa onko merkki iso kirjain. Tässä esimerkki siitä:

```C++
transform(myString.begin(), myString.end(), myString.begin(), [](char c) { return ::isupper(c) ? c : ::tolower(c); });
```

Tämä koodinpätkä muuttaa merkkijonon "TÄMÄ ON ESIMERKKI" muotoon "TÄMÄ on esimerkki".

## Katso myös

- [C++ stringin käsittely](https://www.cs.utexas.edu/~mitra/csFall2017/cs329/lectures/strings.html)
- [C++ transform](https://en.cppreference.com/w/cpp/algorithm/transform)
- [C++ tolower](https://www.cplusplus.com/reference/cctype/tolower/)