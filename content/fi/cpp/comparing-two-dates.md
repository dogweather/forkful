---
title:    "C++: Vertaillaan kahta päivämäärää"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Miksi vertailla kahta päivämäärää?

Vertailemalla kahta päivämäärää, voit tarkistaa niiden välisen eron tai selvittää, kumpi päivämäärä on aikaisempi toiseen nähden. Tämä voi olla erityisen hyödyllistä esimerkiksi tapauksissa, joissa haluat laskea kuinka monta päivää on kulunut tai laskea laskut päivämäärän perusteella.

## Kuinka vertailla päivämääriä C++:ssa?

Vertaillaaksesi kahta päivämäärää C++:ssa, sinun täytyy käyttää aikakirjastoa, joka tarjoaa tarvittavia toimintoja päivämäärien käsittelyyn. Esimerkiksi `chrono`-kirjasto tarjoaa `time_point`-tyypin, jota voidaan käyttää päivämäärien vertailuun.

```C++
#include <iostream>
#include <chrono>
using namespace std;

int main() {
    // Luo kaksi päivämäärää
    chrono::system_clock::time_point date1 = chrono::system_clock::now();
    chrono::system_clock::time_point date2 = date1 - chrono::hours(24);

    // Vertaa päivämääriä
    if (date1 > date2) {
        cout << "Päivämäärä 1 on myöhempi kuin päivämäärä 2" << endl;
    } else if(date1 < date2) {
        cout << "Päivämäärä 1 on aikaisempi kuin päivämäärä 2" << endl;
    } else {
        cout << "Päivämäärät ovat samat" << endl;
    }

    return 0;
}
```

**Tulostus:**

*Päivämäärä 1 on myöhempi kuin päivämäärä 2*

## Syvempi sukellus päivämäärien vertailuun

Päivämäärien vertailu perustuu niiden kokonaislukuesityksiin (engl. integer representation). Tämä tarkoittaa, että jokainen päivämäärä ilmaistaan numeroina, jotka lasketaan tietystä päivästä lähtien. C++:n `chrono`-kirjastossa tämä päivä on **1. tammikuuta 1970**, jota kutsutaan myös vuosisadan alkuajaksi (engl. epoch). Tämän päivän numero on **0** ja sen jälkeen jokaiselle päivälle lasketaan oma numero.

Esimerkiksi, jos haluat vertailla 1. lokakuuta 2021 ja 1. lokakuuta 2020 päivämääriä, voit laskea niiden päivien numerot vuosisadan alkuajasta lähtien:

`1. lokakuuta 2021` = (365 * 51 + 9) = **18728**

`1. lokakuuta 2020` = (365 * 50 + 9) = **18393**

Koska nykyiset päivämäärät tallennetaan 64-bittisiksi luvuiksi C++:ssa, voimme myös vahvistaa tämän laskennan suorittamalla seuraavan komennon:

```C++
chrono::system_clock::now().time_since_epoch().count();
```

**Tulostus:**

*1633050229942346605*

Kuten näemme, tämä luku vastaa laskettua päivämäärän numeroa.

## Katso myös

- https://en.cppreference.com/w/cpp/chrono
- https://www.geeksforgeeks.org/chrono-in-c/
- https://www.learncpp.com/cpp-tutorial/measure-time-in-c/