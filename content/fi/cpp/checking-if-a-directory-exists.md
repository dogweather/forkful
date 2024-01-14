---
title:    "C++: Kansion olemassaolon tarkistaminen"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi

On monia eri syitä, miksi henkilö saattaisi haluta tarkistaa, onko hakemisto olemassa. Yksi yleisimmistä syistä on ohjelman suorittamisen yhteydessä, jolloin on hyödyllistä tietää, onko tietty hakemisto olemassa ennen kuin yritetään suorittaa toimintoja sen kanssa.

## Miten

Onneksi C++:ssa on helppo tarkistaa, onko hakemisto olemassa. Käytämme tähän tarkoitukseen std::filesystem-kirjastoa.

```C++
#include <iostream>
#include <filesystem>
using namespace std::filesystem;

int main() {
  // Tarkistetaan, onko hakemisto "esimerkki" olemassa
  if (exists("esimerkki")) {
    std::cout << "Hakemisto on olemassa!" << std::endl;
  }
  else {
    std::cout << "Hakemistoa ei löydy." << std::endl;
  }
}
```

**Lähtö**: "Hakemisto on olemassa!"

Tässä esimerkissä käytämme exists-funktiota, joka tarkistaa, onko annettu tiedosto tai hakemisto olemassa. Jos haluamme tarkistaa tietyn hakemiston olemassaolon, voimme antaa funktion parametrina hakemiston nimen. Funktio palauttaa totuusarvon, joten voimme käyttää sitä if-lausekkeessa tarkistamaan, toimiiko ehto vai ei.

## Syvällinen tarkastelu

On tärkeää huomata, että exists-funktio ei tarkista pelkästään annettua polkua, vaan se voi myös tarkistaa, onko hakemiston alihakemistoja olemassa. Esimerkiksi, jos käytämme exists-funktiota tarkistamaan, onko hakemisto "esimerkki" olemassa, se voi myös tarkistaa, onko alihakemistoja, kuten "esimerkki/ala", olemassa.

On myös hyvä huomata, että jos käytämme exists-funktiota tarkistamaan, onko tiedosto olemassa, se voi myös tarkistaa, onko kyseinen tiedosto hakemiston alihakemistossa. Esimerkiksi, jos käytämme exists-funktiota tarkistamaan, onko tiedosto "esimerkki.txt" olemassa, se voi myös tarkistaa, löytyykö se hakemiston "esimerkki" alihakemistosta.

## Katso myös

- [std::filesystem-dokumentaatio (cppreference.com)](https://en.cppreference.com/w/cpp/filesystem)
- [Filesystem-ohje (cplusplus.com)](https://www.cplusplus.com/reference/filesystem/)
- [C++:n perustoiminnan opas (opiskele.pdf)](https://www.pdf.investintech.com/preview/84a50422-c211-11e9-89bf-f8bc123048bf/index.html)