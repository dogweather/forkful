---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:14.166338-07:00
description: "Miten: Modernissa C++:ssa (C++17 ja siit\xE4 eteenp\xE4in) voit k\xE4\
  ytt\xE4\xE4 tiedostoj\xE4rjestelm\xE4kirjastoa hakemiston olemassaolon tarkistamiseen.\
  \ Se tarjoaa\u2026"
lastmod: '2024-03-13T22:44:56.879922-06:00'
model: gpt-4-0125-preview
summary: "Modernissa C++:ssa (C++17 ja siit\xE4 eteenp\xE4in) voit k\xE4ytt\xE4\xE4\
  \ tiedostoj\xE4rjestelm\xE4kirjastoa hakemiston olemassaolon tarkistamiseen."
title: Tarkistetaan, onko hakemisto olemassa
weight: 20
---

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
