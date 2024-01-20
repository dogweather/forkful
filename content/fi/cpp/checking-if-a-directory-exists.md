---
title:                "Tarkistetaan, onko hakemisto olemassa"
html_title:           "C: Tarkistetaan, onko hakemisto olemassa"
simple_title:         "Tarkistetaan, onko hakemisto olemassa"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Tarkistetaan, olemassaoleeko kansio koodissa. Näin vältetään virheitä, kun tiedostoja luetaan tai kirjoitetaan. Tärkeää luotettavuuden ja käyttäjäkokemuksen kannalta.

## How to (Kuinka tehdään)
```C++
#include <iostream>
#include <filesystem>

namespace fs = std::filesystem;

int main() {
    fs::path dirPath = "/polun/tarkistettava/kansio";

    if (fs::exists(dirPath)) {
        std::cout << "Kansio on olemassa!" << std::endl;
    } else {
        std::cout << "Kansiota ei löydy." << std::endl;
    }

    return 0;
}
```
**Tulostus:**
```
Kansio on olemassa!
```
tai
```
Kansiota ei löydy.
```

## Deep Dive (Sukellus syvälle)
Tiedostojärjestelmien hallinta on ollut osa C++:aa pitkään, mutta vasta C++17 standardin myötä `std::filesystem` kirjasto tuli standardiksi. Ennen sitä, kehittäjät turvautuivat kolmannen osapuolen kirjastoihin, kuten Boost.Filesystem. Tietojen kirjoittaminen ja lukeminen olemattomiin kansioihin voi johtaa runtime-virheisiin, jotka ovat turhauttavia käyttäjille ja aiheuttavat ohjelman kaatumisia.

C++17-standardi mahdollistaa kansioihin liittyvien toimintojen suorittamisen yhtenäisellä tavalla riippumatta käyttöjärjestelmästä. `std::filesystem`-kirjasto tarjoaa työkalut polkujen hallintaan, tarkasteluun ja manipulointiin. Jos sovelluksesi edellyttää vanhempia C++-standardeja, vaihtoehtona on käyttää `boost::filesystem` tai käyttöjärjestelmäkohtaisia rajapintoja, kuten `stat` POSIX-järjestelmissä tai `GetFileAttributes` Windowsissa.

## See Also (Katso myös)
- C++ Standard Library reference: [std::filesystem](https://en.cppreference.com/w/cpp/filesystem)
- Boost.Filesystem library documentation: [Boost.Filesystem](https://www.boost.org/doc/libs/1_75_0/libs/filesystem/doc/index.htm)
- POSIX 'stat': [POSIX stat](https://pubs.opengroup.org/onlinepubs/009695399/functions/stat.html)