---
title:                "Tarkistetaan, onko hakemisto olemassa"
html_title:           "C++: Tarkistetaan, onko hakemisto olemassa"
simple_title:         "Tarkistetaan, onko hakemisto olemassa"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Tarkistaminen, onko kansio olemassa, on menetelmä selvittää, löytyyko järjestelmästä tiettyä kansiota. Ohjelmoijat tekevät sen tavallisesti ennen kuin he kirjoittavat tiedostoja kyseiseen kansioon, jotta he voivat käsitellä virheitä paremmin.

## Miten Tehdä:
Voit tarkistaa, onko kansio olemassa, käyttäen `std::filesystem` kirjastoa. Tässä on esimerkki:

```C++
#include <filesystem>

bool directoryExists(const std::string& dir) {
    return std::filesystem::exists(dir);
}

int main() {
   std::string dir = "/path/to/directory";
   if(directoryExists(dir)) {
       std::cout << "Directory exists\n";
   } else {
       std::cout << "Directory doesn't exist\n";
   }

   return 0;
}
```
Jos kansio löytyy, ohjelma tulostaa "Directory exists". Jos kansiota ei löydy, tulostuu "Directory doesn't exist".

## Syvällinen Katsaus:
Historiallisesti tarkistaaksemme, onko kansio olemassa, usein käytettiin `stat` funktiota POSIX standardista. C++17 lisäsi kuitenkin `std::filesystem` kirjaston, joka tarjoaa yksinkertaisen ja tehokkaan tavan tällaiseen toimintaan.

Vaihtoehtoisesti voit käyttää myös `boost::filesystem` -kirjastoa, joka on samanlainen kuin `std::filesystem`, mutta toimii vanhemmilla C++ standardeilla.

Toteuttaessa, `std::filesystem::exists` funktio tarkistaa tiedoston tai kansion olemassaolon polun avulla. Se palauttaa `false`, jos polku ei ole olemassa tai jos tapahtui virhe (esim. jos sinulla ei ole riittäviä oikeuksia).

## Katso Myös:
Lisätietoja voit löytää seuraavista lähteistä:
- [Boost.Filesystem library](https://www.boost.org/doc/libs/1_75_0/libs/filesystem/doc/index.htm)