---
title:    "C++: Hakeminen ja tekstin korvaaminen"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Miksi hakea ja korvata tekstiä?

Hakea ja korvata tekstiä voi olla hyödyllistä silloin, kun haluat muuttaa suuria määriä tekstiä kooditiedostoissasi. Sen avulla voit nopeasti päivittää kaikki esiintymät haluamallasi tavalla, säästäen siten aikaa ja vaivaa.

## Kuinka suorittaa haku ja korvaus

```
#include <iostream>
#include <string>

using namespace std;

int main() {
    // Alustetaan muuttujat
    string teksti = "Tämä on esimerkki teksti";
    string etsittava = "esimerkki";
    string korvaaja = "harjoitus";
   
    //Suoritetaan haku ja korvaus
    size_t indeksi = teksti.find(etsittava);
    if (indeksi != string::npos) {
        teksti.replace(indeksi, etsittava.length(), korvaaja);
    }

    // Tulostetaan tulos
    cout << teksti << endl;

    return 0;
}
```

Output: Tämä on harjoitus teksti

Tässä esimerkissä käytetään string-luokan metodeja "find" ja "replace" hakemaan ja korvaamaan haluttu teksti. Ensimmäisessä vaiheessa tallennetaan etsittävä ja korvaava teksti erillisiin muuttujiin, jotta sitä voi helposti muuttaa tarvittaessa. Sitten suoritetaan haku "find" -metodilla ja tarkistetaan, että etsitty teksti löytyy. Jos se löytyy, se korvataan "replace" -metodilla uudella tekstillä. Lopuksi tulostetaan muutettu teksti.

## Syvällinen sukellus hakuun ja korvaukseen

Haku ja korvaus tekstissä on hyvin yksinkertainen toimenpide, mutta siitä on hyötyä, kun tarvitset tehdä samanlaisia muutoksia useissa tiedostoissa. Voit käyttää myös säännöllisiä lausekkeita hakeaksesi ja korvataksesi tekstiä monimutkaisemmin.

Voit myös tehdä useita hakuja ja korvauksia samanaikaisesti, korvaamalla tekstiä eri tavoin eri tiedostoissa. Tämä on kätevä ominaisuus, kun teet laajempia muutoksia koodissasi.

## Katso myös

- [C++ string -luokka](https://www.cplusplus.com/reference/string/string/)
- [Haku ja korvaus C++:ssa](https://www.geeksforgeeks.org/search-replace-c/)
- [Säännölliset lausekkeet C++:ssa](https://www.regular-expressions.info/cplusplus.html)