---
title:                "C++: Tarkista onko hakemistoa olemassa"
simple_title:         "Tarkista onko hakemistoa olemassa"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi: Tarkistaminen onko hakemisto olemassa?

On monia tilanteita, joissa ohjelmoija haluaa tarkistaa, onko tietty hakemisto olemassa. Esimerkiksi tietyn tiedoston avaaminen voi vaatia, että sen sisältämä hakemisto on olemassa. Hakemiston tarkistaminen auttaa myös välttämään virheitä ohjelman suorituksessa.

## Kuinka:

Käytännössä hakemiston olemassaolon tarkistaminen on helppoa C++:ssa käyttäen `std::filesystem` -kirjastoa. Tarvittava kirjasto sisältyy C++17 -standardiin, joten sen ei pitäisi olla ongelma nykyaikaisissa kääntäjissä.

Alla on esimerkkikoodi, jossa tarkistetaan, onko hakemisto nimeltä "kuvat" olemassa:

```C++
#include <iostream>
#include <filesystem>

namespace fs = std::filesystem;

int main() {
    fs::path polku("kuvat");
    if (fs::exists(polku)) {
        std::cout << "Hakemisto kuvat on olemassa." << std::endl;
    } else {
        std::cout << "Hakemisto kuvat ei ole olemassa." << std::endl;
    }
    return 0;
}
```

Esimerkkituloste:

```
Hakemisto kuvat on olemassa.
```

## Syvyyteen:

Tarkistamisen taustalla on käyttöjärjestelmän tiedostojärjestelmän toiminta. Kun hakemistoa etsitään, tiedostojärjestelmä käy läpi hakemistorakennetta alkaen juurihakemistosta ja tarkistaen jokaisen kansion ja tiedoston olemassaolon. Jos haettua hakemistoa ei löydy, tiedostojärjestelmä palauttaa virheen.

On myös tärkeä huomata, että pelkkä hakemiston olemassaolo ei takaa sitä, että kyseinen hakemisto on käytettävissä. Esimerkiksi käyttöoikeudet tai tallennustila voivat estää ohjelmaa pääsemästä hakemistoon.

## Katso myös:

- [std::filesystem -referenssi](https://en.cppreference.com/w/cpp/filesystem)
- [Tiedostojärjestelmä C++17:ssä](https://en.cppreference.com/w/cpp/filesystem/fs)