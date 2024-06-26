---
date: 2024-01-20 17:40:16.048506-07:00
description: "How to: (Kuinka tehd\xE4\xE4n:) C++17 toi mukanaan `<filesystem>` kirjaston,\
  \ mik\xE4 helpottaa v\xE4liaikaisten tiedostojen k\xE4sittely\xE4."
lastmod: '2024-04-05T21:53:58.462861-06:00'
model: gpt-4-1106-preview
summary: "(Kuinka tehd\xE4\xE4n:) C++17 toi mukanaan `<filesystem>` kirjaston, mik\xE4\
  \ helpottaa v\xE4liaikaisten tiedostojen k\xE4sittely\xE4."
title: "V\xE4liaikaistiedoston luominen"
weight: 21
---

## How to: (Kuinka tehdään:)
C++17 toi mukanaan `<filesystem>` kirjaston, mikä helpottaa väliaikaisten tiedostojen käsittelyä.

```C++
#include <filesystem>
#include <fstream>
#include <iostream>

int main() {
    // Luo väliaikainen tiedosto unique temp -nimessä
    std::filesystem::path temp_path = std::filesystem::temp_directory_path() / "example.txt";

    // Kirjoita tiedosto
    {
        std::ofstream temp_file(temp_path);
        temp_file << "Väliaikaista dataa.\n";
        std::cout << "Tiedosto luotu: " << temp_path << std::endl;
    } // ofstream tuhotaan, tiedosto sulkeutuu

    // Lue ja tulosta tiedoston sisältö
    {
        std::ifstream temp_file(temp_path);
        std::cout << "Tiedoston sisältö: " << temp_file.rdbuf();
    }

    // Poista väliaikainen tiedosto
    std::filesystem::remove(temp_path);
    std::cout << "Tiedosto poistettu: " << temp_path << std::endl;

    return 0;
}
```
Koodi luo, kirjoittaa, lukee ja lopulta poistaa väliaikaisen tiedoston.

## Deep Dive (Sukellus syvyyksiin):
Väliaikaisia tiedostoja on käytetty niin kauan kuin tietokoneet ovat olleet olemassa. Ne tarjoavat tapa käsitellä tietoja ilman pelkoa pysyvistä muutoksista levylle. Ennen `<filesystem>` kirjastoa kehittäjät turvautuivat `tmpfile()` ja `mkstemp()` C-funktioihin tai käyttivät kolmansien osapuolten kirjastoja.

Vaihtoehtoisia tapoja luoda väliaikaisia tiedostoja ovat esimerkiksi Boost.Filesystem kirjasto tai suoraan käyttöjärjestelmän tarjoamat API:t. Implementaatioissa on eroavaisuuksia: jotkin poistavat tiedoston automaattisesti, toiset vaativat käyttäjän toimia.

Varmuuden, datan eheyden ja turvallisuuden takaamiseksi oikein käsitellyt väliaikaiset tiedostot ovat keskeisiä. Koodi ei saisi koskaan olettaa, että tiedosto on väliaikainen ilman asianmukaisia toimenpiteitä.

## See Also (Katso myös):
- C++ Standard Library Reference: https://en.cppreference.com/w/cpp/filesystem
- Boost.Filesystem Library: https://www.boost.org/doc/libs/release/libs/filesystem/
- POSIX standard functions for temporary files (for historical context): https://pubs.opengroup.org/onlinepubs/9699919799/functions/mkstemp.html
