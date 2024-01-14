---
title:                "C++: Väliaikaisen tiedoston luominen"
simple_title:         "Väliaikaisen tiedoston luominen"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Miksi luoda väliaikainen tiedosto?

On monia syitä, miksi ohjelmoijat luovat väliaikaisia tiedostoja. Yleisimpiä syitä ovat tiedon tallentaminen, väliaikaisen datan käyttö ja tarvittavan resurssin varaus. Näitä tiedostoja käytetään yleensä ohjelman suorituksen aikana ja poistetaan lopuksi suorituksen päätyttyä.

## Kuinka luoda väliaikainen tiedosto

Väliaikaisen tiedoston luominen C++:lla on helppoa. Alla on esimerkki koodista, jolla luodaan väliaikainen tiedosto "temp.txt" ja kirjoitetaan siihen tietoa:

```c++
#include <fstream>
using namespace std;

int main() {
// luodaan ja avataan väliaikainen tiedosto
ofstream temp_file("temp.txt");

// tietojen kirjoittaminen tiedostoon
temp_file << "Tämä on väliaikainen tiedosto" << endl;

// tiedoston sulkeminen ja poistaminen
temp_file.close();
remove("temp.txt");

return 0;
}
```

Saatu tuloste on:

```
Tämä on väliaikainen tiedosto
```

Koodissa käytetään fstream-kirjastoa, joka mahdollistaa tiedoston luomisen ja tietojen kirjoittamisen siihen. Lopuksi käytetään remove-funktiota poistamaan väliaikainen tiedosto.

## Syvällisempi tarkastelu väliaikaisen tiedoston luomisesta

Väliaikaiset tiedostot luodaan yleensä käyttöjärjestelmän temp-hakemistoon, joka on varattu väliaikaisten tiedostojen tallentamiseen. Jokaisella käyttöjärjestelmällä on oma temp-hakemistonsa, joka löytyy tiedostojärjestelmän juuresta.

Väliaikainen tiedosto voidaan luoda myös käyttämällä temporary_filestream-luokkaa fstream-kirjastossa. Tämä mahdollistaa automatisoidun tiedoston poistamisen sen sulkemisen yhteydessä, mikä on kätevä vaihtoehto erityisesti suurten tiedostojen käsittelyssä.

## Katso myös

- [fstream-kirjaston dokumentaatio](http://www.cplusplus.com/reference/fstream/)
- [temporary_filestream-luokan dokumentaatio](http://www.cplusplus.com/reference/fstream/temporary_filestream/)