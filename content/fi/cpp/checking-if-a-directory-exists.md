---
title:                "Tarkista onko hakemistoa olemassa"
html_title:           "C++: Tarkista onko hakemistoa olemassa"
simple_title:         "Tarkista onko hakemistoa olemassa"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Tiedoston hakemiston tarkistaminen on prosessi, jossa ohjelmoijat tarkistavat, onko tietyssä sijainnissa oleva tiedosto tai hakemisto olemassa. Tämä on erittäin tärkeä askel, koska se auttaa estämään virheitä ja toimintahäiriöitä, kun ohjelma käsittelee tiedostoja ja hakemistoja. Ohjelmoijat tekevät tämän varmistaakseen, että heidän ohjelmansa suorittaa oikeat toiminnot ja käsittelee tietoja oikein.

## Kuinka tehdä se:
```C++
#include <iostream>
#include <filesystem>

int main() {
    std::filesystem::path dirPath = "C:/Users/Käyttäjä/Tiedostot";
    if (std::filesystem::exists(dirPath)) {
        std::cout << "Hakemisto on olemassa." << std::endl;
    }
    else {
        std::cout << "Hakemistoa ei löydy." << std::endl;
    }
    return 0;
}
```
Esimerkissä käytetään ```<filesystem>``` kirjastoa, joka on uusi C++17 standardin mukainen tapa käsitellä tiedostoja ja hakemistoja. Funktio ```exists()``` tarkistaa, onko annettu polku olemassa ja palauttaa bool-arvon. Tämän jälkeen tulostetaan vastaava viesti.

Koodin tulos voisi olla seuraavanlainen:
```
Hakemisto on olemassa.
```

## Syväsukellus:
Tiedostojen ja hakemistojen käsittely on ollut osa ohjelmointia jo pitkään. Aiemmin ohjelmoijat joutuivat käyttämään mm. C:n ```<dirent.h>``` kirjastoa, joka ei ole yhtä käyttäjäystävällinen ja monimutkainen. Uudemmat C++ versiot ovat helpottaneet tätä prosessia kehittämällä uusia menetelmiä ja kirjastoja, kuten ```<filesystem>```.

On myös olemassa muita tapoja tarkistaa hakemistoja, kuten käyttää POSIX:n ```stat()``` funktiota tai antaa käyttäjän syöttää polku itsenäisesti ```cin``` komennolla. Nämä menetelmät eivät kuitenkaan ole yhtä tehokkaita ja suositeltavia kuin ```<filesystem>``` kirjasto.

## Katso myös:
- [C++17 <filesystem>](https://en.cppreference.com/w/cpp/filesystem)
- [dirent.h kirjasto](https://www.gnu.org/software/libc/manual/html_node/Reading_002fClosing_002fRemoving-Directory.html)