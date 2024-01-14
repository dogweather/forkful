---
title:    "C++: Tiedoston kirjoittaminen"
keywords: ["C++"]
---

{{< edit_this_page >}}

# Miksi Kirjoittaa Tekstitiedosto?

Joskus ohjelmoidessa joudumme tallentamaan tekstiä tietokoneelle. Tämä voi olla esimerkiksi käyttäjän antama syöte tai tärkeä tieto, jota haluamme säilyttää ohjelman jälkeen. Tällöin on hyödyllistä osata kirjoittaa tekstitiedostoja C++:ssa.

## Kuinka Tehdä Se

Kirjoittaaksesi tekstitiedoston C++:ssa, sinun tulee ensin avata tiedosto ja määrittää sen sijainti ja tiedostotyyppi. Tämän jälkeen voit käyttää ```std::ofstream```-luokkaa kirjoittaaksesi sisältöä tiedostoon.

```
#include <iostream>
#include <fstream>

int main() {
    // Määritetään tiedoston sijainti ja tiedostotyyppi
    std::ofstream tiedosto("tiedosto.txt");

    // Testidataa
    std::string nimi = "Matti";
    int ikä = 25;

    // Kirjoitetaan tiedostoon
    tiedosto << "Tervetuloa, " << nimi << "! Olet " << ikä << " vuotta vanha." << std::endl;

    // Suljetaan tiedosto
    tiedosto.close();

    return 0;
}
```

## Syöte ja Tulos

Ohjelman suorituksen jälkeen löydät tekstitiedoston nimeltä "tiedosto.txt" tiedostosi kansiosta. Kun avaat tiedoston, näet seuraavan tulosteen:

```
Tervetuloa, Matti! Olet 25 vuotta vanha.
```

## Syvällisempi Tarkastelu

Kirjoittaessa tekstitiedostoa C++:ssa, voit käyttää myös muita metodeja kuten ```write()``` ja ```put()``` kirjoittaaksesi tiedostoon. Voit myös käyttää ```seekp()```-funktiota määrittääksesi sijainnin, josta haluat kirjoittaa tiedostoon.

# Katso myös

- [C++ ofstream-luokka selityksineen](https://www.cplusplus.com/reference/fstream/ofstream/)
- [Ohjeita tekstitiedoston kirjoittamiseen C++:ssa](https://www.geeksforgeeks.org/writing-text-file-c/)

 end.