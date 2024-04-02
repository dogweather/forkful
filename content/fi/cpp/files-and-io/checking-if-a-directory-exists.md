---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:14.166338-07:00
description: "Hakemiston olemassaolon tarkistaminen tarkoittaa m\xE4\xE4ritt\xE4mist\xE4\
  , onko tietyss\xE4 polussa hakemistoa ennen toimintojen, kuten tiedostojen lukemisen\
  \ tai niihin\u2026"
lastmod: '2024-03-13T22:44:56.879922-06:00'
model: gpt-4-0125-preview
summary: "Hakemiston olemassaolon tarkistaminen tarkoittaa m\xE4\xE4ritt\xE4mist\xE4\
  , onko tietyss\xE4 polussa hakemistoa ennen toimintojen, kuten tiedostojen lukemisen\
  \ tai niihin\u2026"
title: Tarkistetaan, onko hakemisto olemassa
weight: 20
---

## Mikä & Miksi?
Hakemiston olemassaolon tarkistaminen tarkoittaa määrittämistä, onko tietyssä polussa hakemistoa ennen toimintojen, kuten tiedostojen lukemisen tai niihin kirjoittamisen, suorittamista. Ohjelmoijat tekevät sen välttääkseen tiedosto-operaatioihin liittyviä virheitä, varmistaen sujuvamman ja luotettavamman tiedostonkäsittelyn tehtävien suorituksen sovelluksissaan.

## Miten:
Modernissa C++:ssa (C++17 ja siitä eteenpäin) voit käyttää tiedostojärjestelmäkirjastoa hakemiston olemassaolon tarkistamiseen. Se tarjoaa suoraviivaisen ja standardoidun tavan suorittaa tiedostojärjestelmäoperaatioita, mukaan lukien hakemiston olemassaolon tarkistaminen.

```cpp
#include <iostream>
#include <filesystem>

namespace fs = std::filesystem;

int main() {
    const fs::path dirPath = "/path/to/directory";

    if (fs::exists(dirPath) && fs::is_directory(dirPath)) {
        std::cout << "Hakemisto on olemassa." << std::endl;
    } else {
        std::cout << "Hakemistoa ei ole olemassa." << std::endl;
    }

    return 0;
}
```
Näyte tulosteesta, jos hakemisto on olemassa:
```
Hakemisto on olemassa.
```

Näyte tulosteesta, jos hakemistoa ei ole olemassa:
```
Hakemistoa ei ole olemassa.
```

Projekteille, jotka eivät vielä käytä C++17:ää tai tarvitsevat lisäominaisuuksia, Boost Filesystem -kirjasto on suosittu kolmannen osapuolen valinta, joka tarjoaa samanlaisen toiminnallisuuden.

```cpp
#include <iostream>
#include <boost/filesystem.hpp>

namespace fs = boost::filesystem;

int main() {
    const fs::path dirPath = "/path/to/directory";

    if (fs::exists(dirPath) && fs::is_directory(dirPath)) {
        std::cout << "Hakemisto on olemassa." << std::endl;
    } else {
        std::cout << "Hakemistoa ei ole olemassa." << std::endl;
    }

    return 0;
}
```
Boost Filesystemin käyttöä, tuloste olisi identtinen C++17 tiedostojärjestelmäesimerkin kanssa, riippuen hakemiston olemassaolosta määritetyssä polussa.
