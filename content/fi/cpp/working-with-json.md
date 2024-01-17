---
title:                "Työskentely jsonin kanssa"
html_title:           "C++: Työskentely jsonin kanssa"
simple_title:         "Työskentely jsonin kanssa"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/working-with-json.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Työskentely JSONin kanssa tarkoittaa datan tallentamista ja jakamista selkeässä ja helposti luettavassa muodossa. Ohjelmoijat käyttävät JSONia, koska se on kevyt, helppo ymmärtää ja yleisesti tuettu.

## Miten:
Koodiesimerkkejä ja esimerkkitulosteita on esitetty ```C++ ... ```-koodilohkoissa.

Esimerkki JSON-tiedoston lukemisesta ja tiedon tallentamisesta:

```C++
#include <iostream>
#include <fstream>
#include <jsoncpp/json/json.h>

int main()
{
    // Luodaan JSON-tiedoston lukemista varten
    std::ifstream file("esimerkki.json");

    // Alustetaan Json-lukija ja varataan muistia
    Json::Reader reader;
    Json::Value root;

    // Luetaan tiedosto ja tallennetaan data muuttujaan 'root'
    reader.parse(file, root);

    // Tulostetaan JSON-tiedoston sisältö
    std::cout << "Nimi: " << root["nimi"].asString() << std::endl;
    std::cout << "Ikä: " << root["ikä"].asInt() << std::endl;
    std::cout << "Harrastukset:" << std::endl;
    
    // Käydään läpi kaikki JSON-tiedostossa olevat harrastukset
    for (int i = 0; i < root["harrastukset"].size(); i++)
    {
        std::cout << "- " << root["harrastukset"][i].asString() << std::endl;
    }

    return 0;
}
```
**Esimerkkitulos:**
```
Nimi: Maija Meikäläinen
Ikä: 27
Harrastukset:
- Valokuvaus
- Lukeminen
- Pianonsoitto
```
## Syvempi sukellus:
JSON, eli JavaScript Object Notation, on dataformaatti, joka kehitettiin helpottamaan datan tallentamista ja jakamista verkossa. Se on tullut yhdeksi suosituimmista tavoista tallentaa dataa sovelluksissa ja web-palveluissa. JSON on helppo lukea ja ymmärtää sekä sen syntaksi on yksinkertainen, mikä tekee siitä hyvin tuetun ja käytetyn formaatin.

Vaihtoehtoisia tapoja tallentaa dataa ovat esimerkiksi XML tai CSV. Näihin verrattuna JSON on kevyempi ja helpompi lukea ja muokata.

JSONin käyttöönotto C++:ssa vaatii lisäkirjaston, esimerkiksi jsoncpp-kirjaston, käyttöönoton. Kirjasto sisältää valmiita työkaluja JSON-tiedostojen lukemiseen ja kirjoittamiseen.

## Katso myös:
- [JSON for Modern C++](https://github.com/nlohmann/json) - Suosittu JSON-kirjasto C++-ohjelmoijille.
- [Introducing JSON](https://www.json.org/) - JSON-oppaasta voit löytää lisätietoa JSONin syntaksista ja sen käytöstä.
- [JSON Validator](https://jsonlint.com/) - Työkalu, jolla voit tarkistaa onko JSON-koodisi oikein muotoiltu.