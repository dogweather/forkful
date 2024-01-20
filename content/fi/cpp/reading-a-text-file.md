---
title:                "Tekstitiedoston lukeminen"
html_title:           "Lua: Tekstitiedoston lukeminen"
simple_title:         "Tekstitiedoston lukeminen"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Tekstin lukemisen ohjelmointi C++:ssa

## Mikä & Miksi?

Tekstitiedoston lukeminen on prosessi, jossa ohjelma noutaa ja käsittelee tiedossa olevia tietoja. Ohjelmoijat tekevät sen tiedon käyttämiseksi, analysoimiseksi ja manipuloimiseksi.

## Näin se tehdään:

```C++
#include <fstream>
#include <string>
#include <iostream>

int main() {
    std::ifstream file("example.txt");
    std::string line;

    if (!file.is_open()) {
        std::cout << "Tiedoston avaaminen epäonnistui." << std::endl;
        return 1;
    }

    while (std::getline(file, line)) {
        std::cout << line << std::endl;
    }

    file.close();

    return 0;
}
```
Otoksen tuotoksessa lukee rivejä `example.txt` -nimisestä tiedostosta ja tulostaa ne konsoliin.

## Sukellus syvemmälle

Tekstitiedostojen lukemisen historia kulkee käsi kädessä tietokoneiden kanssa. Varhaisin esimerkki tällaisesta on lochkortit - tyypillisesti pahvista valmistettu kortti, jossa on esipainetut tietueet tiedon tallentamiseksi.

Vaihtoehtoja tiedostojen käsittelyyn C++:ssa ovat mm. `fread()` ja `fstream`. `fread()` on C-kielen funktio, normaalisti käytetty binääritiedostojen käsittelyyn. `fstream` on C++ kirjaston tiedostonkäsittelyluokka, jota käytettiin edellä olevassa esimerkissä.

Tekstitiedoston lukemisessa tietojen siirtäminen levyltä muistiin (I/O-toiminnot) saattaa olla hitaampaa kuin levyn lukuoperaatiot. Tämä johtuu usein käyttöjärjestelmän suorittamista välivarastointitoiminnoista (caching/buffering), jotka yrittävät parantaa levyoperaatioiden suorituskykyä. Näinollen useita pieniä lukuoperaatioita on yleensä hitaampaa kuin yksi suuri operaatio.

## Katso myös

* [Cplusplus.com - std::ifstream](http://www.cplusplus.com/reference/fstream/ifstream/) : Perusteellinen tietopaketti C++:n `ifstream`-luokasta.
* [Cppreference.com - Input/Output Library](https://en.cppreference.com/w/cpp/io) : Yksityiskohtainen C++:n I/O-kirjaston kuvaus.
* [GeeksforGeeks - C++ File Handling](https://www.geeksforgeeks.org/c-plus-plus-file-handling/?ref=lbp) : Paljon esimerkkejä ja vinkkejä tekstitiedostojen käsittelystä C++:ssa.