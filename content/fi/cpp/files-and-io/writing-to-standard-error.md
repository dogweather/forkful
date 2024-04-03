---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:40.336820-07:00
description: "Kuinka: C++:ssa standardivirheeseen kirjoittaminen voidaan saavuttaa\
  \ k\xE4ytt\xE4m\xE4ll\xE4 `cerr`-virtaa, joka on osa standardikirjastoa. T\xE4ss\xE4\
  \ on perusesimerkki."
lastmod: '2024-03-13T22:44:56.881849-06:00'
model: gpt-4-0125-preview
summary: "C++:ssa standardivirheeseen kirjoittaminen voidaan saavuttaa k\xE4ytt\xE4\
  m\xE4ll\xE4 `cerr`-virtaa, joka on osa standardikirjastoa."
title: Kirjoittaminen standardivirheeseen
weight: 25
---

## Kuinka:
C++:ssa standardivirheeseen kirjoittaminen voidaan saavuttaa käyttämällä `cerr`-virtaa, joka on osa standardikirjastoa. Tässä on perusesimerkki:

```cpp
#include <iostream>

int main() {
    // Kirjoittaminen standarditulosteeseen
    std::cout << "Tämä on normaali viesti." << std::endl;
    
    // Kirjoittaminen standardivirheeseen
    std::cerr << "Tämä on virheviesti." << std::endl;
    
    return 0;
}
```

Esimerkkituloste:
```
Tämä on normaali viesti.
Tämä on virheviesti.
```

Tässä tapauksessa molemmat viestit ilmestyvät tyypillisesti terminaaliisi, mutta voit ohjata ne erikseen komentorivillä. Esimerkiksi, voit lähettää standarditulosteen tiedostoon samalla kun annat virheiden näkyä näytöllä.

Monimutkaisempaan lokitukseen ja virheenkäsittelyyn voidaan käyttää kolmannen osapuolen kirjastoja, kuten `spdlog` tai `boost.log`. Nämä kirjastot tarjoavat parannettuja ominaisuuksia lokitukseen, mukaan lukien muotoilu, lokitasot ja tiedostotulostus.

Tässä on esimerkki siitä, miten saatat käyttää `spdlog`ia kirjoittaaksesi virheviestin:

```cpp
#include "spdlog/spdlog.h"

int main() {
    // Alusta spdlog
    spdlog::info("Tämä on normaali viesti.");
    spdlog::error("Tämä on virheviesti.");
    
    return 0;
}
```

Huom: `spdlog`in käyttämiseksi sinun täytyy lisätä se projektiisi. Voit tehdä tämän kloonaamalla varaston GitHubista tai käyttämällä paketinhallintajärjestelmää kuten `vcpkg` tai `conan`. 

Muista, että valinta suorien standardivirtojen tai kirjaston kuten `spdlog` käytön välillä riippuu sovelluksesi monimutkaisuudesta ja erityistarpeistasi virheenkäsittelyn ja lokituksen suhteen.
