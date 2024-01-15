---
title:                "Tiedoston kirjoittaminen"
html_title:           "C++: Tiedoston kirjoittaminen"
simple_title:         "Tiedoston kirjoittaminen"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Kirjoittaessasi C++-ohjelmointikieltä, sinun saatat joutua kirjoittamaan tekstitiedostoja. Usein sinun täytyy tallentaa tietoja tiedostoihin, jotka voidaan lukea ja muokata myöhemmin. Tekstitiedostot ovat hyödyllisiä monissa ohjelmoinnin sovelluksissa, kuten tietojen tallennuksessa ja tiedostojen luomisessa.

## Miten

Tekstitiedoston luominen C++: lla on yksinkertaista. Sinun täytyy vain avata tiedosto, kirjoittaa tiedostoon haluamasi tiedot ja sulkea tiedosto. Käytä "ofstream" -luokkaa avataksesi tiedoston ja "<<" -toimintoa kirjoittaaksesi tiedot. Alla on esimerkki koodista, joka luo uuden tekstitiedoston nimeltä "esimerkki.txt" ja kirjoittaa siihen "Tervetuloa kirjoittamaan tekstitiedostoja C++: lla!".

```
#include <iostream>
#include <fstream>

int main()
{
  // Avaa tiedosto "esimerkki.txt"
  std::ofstream tiedosto("esimerkki.txt");

  // Kirjoita tiedostoon haluamasi teksti
  tiedosto << "Tervetuloa kirjoittamaan tekstitiedostoja C++: lla!";

  // Sulje tiedosto
  tiedosto.close();

  return 0;
}
```

Suorittaessa koodia, huomaat, että "esimerkki.txt" -tiedosto on luotu samassa kansiossa kuin ohjelmasi ja siinä on siihen kirjoittamasi teksti.

## Syvempi sukellus

Teksti tiedostojen kirjoittamiseen C++: ssa on myös muita vaihtoehtoja, kuten "fopen" -toiminto tai "putchar" -toiminto yksittäisten merkkien kirjoittamiseksi tiedostoon. Voit myös käyttää "stringstream" -luokkaa muokataksesi ja tallentaaksesi erilaisia tietotyyppejä tiedostoon.

## Katso myös

- [C++ ofstream-luokka](https://www.cplusplus.com/reference/fstream/ofstream/)
- [C++ stringstream-luokka](https://www.cplusplus.com/reference/sstream/stringstream/)
- [C++ tiedostojen avaaminen ja lukeminen](https://www.tutorialspoint.com/cplusplus/cpp_files_streams.htm)