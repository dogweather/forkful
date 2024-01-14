---
title:                "C++: Tekstitiedoston kirjoittaminen"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Miksi kirjoittaisit tekstitiedoston? Tekstitiedostot ovat hyödyllisiä kun haluat tallentaa tai lukea käyttöjärjestelmän ulkopuolisen tiedoston tietoja.

## Miten

```C++
#include <iostream>
#include <fstream>

int main()
{
    // Avataan tiedosto nimeltä "esimerkki.txt" ja kirjoitetaan siihen tekstiä
    std::ofstream tiedosto("esimerkki.txt");
    tiedosto << "Tämä on esimerkkitekstiä!" << std::endl;
    
    // Suljetaan tiedosto
    tiedosto.close();

    // Avataan tiedosto uudelleen ja luetaan siitä tekstiä
    std::ifstream tiedosto2("esimerkki.txt");
    if (tiedosto2.is_open()) {
        std::string rivi;
        while (getline(tiedosto2, rivi)) {
            std::cout << rivi << std::endl;
        }
        tiedosto2.close();
    }
    else {
        std::cout << "Tiedoston avaaminen epäonnistui!" << std::endl;
        return 1;
    }

    return 0;
}
```

Esimerkissä avataan tiedosto nimeltä "esimerkki.txt" ja kirjoitetaan siihen tekstiä. Sen jälkeen tiedosto suljetaan. Tämän jälkeen tiedosto avataan uudelleen ja siitä luetaan tekstiä. Lopuksi tiedosto suljetaan ja ohjelma palaa takaisin pääfunktioon.

## Syväsukellus

Tekstitiedostojen kirjoittaminen on olennainen osa monia C++ ohjelmia. Tiedostojen luominen, avaaminen, kirjoittaminen ja lukeminen ovat kaikki tärkeitä taitoja, jotka tulevat tutuiksi tekstitiedostoja käsitellessä. Tekstien lisäksi tiedostoihin voi myös tallentaa muita datatyyppejä, kuten lukuja ja taulukoita. On myös tärkeää huolehtia tiedoston käsittelyn oikeasta järjestyksestä, jotta tiedostoa voidaan käyttää halutulla tavalla.

## Katso myös

- [C++ tiedostojen käsittely](https://www.cplusplus.com/doc/tutorial/files/)
- [Tiedostojen käsittely C++:lla](https://www3.ntu.edu.sg/home/ehchua/programming/cpp/cp10_IO.html)
- [C++ tiedostot](https://www.tutorialspoint.com/cplusplus/cpp_files_streams.htm)