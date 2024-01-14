---
title:                "C++: Kansion olemassaolon tarkistaminen"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

Miksi: Tämä blogikirjoitus tarkastelee, miksi ja miten tarkistaa, onko hakemisto olemassa, ja syventyy tarkempaan tietoon aiheesta.

Miksi:

Monissa C++-ohjelmoinnin tehtävissä, esimerkiksi tietokantojen tai tiedostojen käsittelyssä, on tärkeää varmistaa, että tietty hakemisto tai polku on olemassa ennen tiedoston avaamista tai tallentamista. Tämä auttaa välttämään mahdollisia virheitä tai poikkeuksia, jotka voivat aiheuttaa ohjelman kaatumisen.

Miten:

Tarkistaaksesi hakemiston olemassaolon C++:lla, voit käyttää "C++ #include <dirent.h> " kirjastoa sekä "opendir" ja "closedir" funktioita. Seuraava koodiesimerkki osoittaa yksinkertaisen tavan tarkistaa, onko hakemisto olemassa ja tulostaa tulos näytölle:

```C++
#include <iostream>
#include <dirent.h>

int main() {
    DIR *dir = opendir("tarkistettava_hakemisto"); // avataan hakemisto
    if (dir) { // tarkistetaan, onko hakemisto olemassa
        std::cout << "Hakemisto on olemassa." << std::endl;
        closedir(dir); // suljetaan hakemisto
    } else { // jos hakemistoa ei ole olemassa
        std::cout << "Hakemistoa ei ole olemassa." << std::endl;
    }
    return 0;
}
```

Esimerkkituloste:

```
Hakemisto on olemassa.
```

Deep Dive:

Tarkistaessaan hakemiston olemassaoloa, ohjelma käytännössä yrittää avata kyseistä hakemistoa ja tarkistaa, onko se avattavissa vai saadaanko siitä virheilmoitus. Avattaessa hakemisto, "opendir" funktio palauttaa osoittimen "DIR" tyyppiseen rakenteeseen, joka edustaa avattua hakemistoa. Jos hakemistoa ei löydy, "opendir" palauttaa "NULL" osoittimen ja virheilmoituksen.

On myös hyvä huomata, että "opendir" funktio hyväksyy sekä absoluuttisen että suhteellisen polun hakemistoon. Jos siis haluat tarkistaa jonkin muun hakemiston olemassaolon kuin nykyisen työhakemiston, voit antaa sille täyden polun.

See Also:

- UNIX Systems Programming with C++
https://www.ibm.com/developerworks/aix/library/au-unixcpp.html
- C++ Reference: opendir()
https://www.cplusplus.com/reference/cstdio/opendir/
- C++ Filesystem: exists()
https://en.cppreference.com/w/cpp/filesystem/exists