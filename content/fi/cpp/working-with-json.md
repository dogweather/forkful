---
title:                "JSON-tiedostojen käsittely"
date:                  2024-01-19
simple_title:         "JSON-tiedostojen käsittely"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/working-with-json.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
JSON on kevyt dataformaatti tiedon vaihtoon. Ohjelmoijat käyttävät JSONia koska se on helppolukuinen sekä ihmisille että koneille, ja yhteensopiva useiden ohjelmointikielien kanssa.

## Miten:
```C++
#include <iostream>
#include <nlohmann/json.hpp> // Kirjasto JSONin käsittelyyn

int main() {
    // JSON-objektin luominen
    nlohmann::json j;
    j["nimi"] = "Maija";
    j["ikä"] = 30;
    j["kielitaito"] = {"suomi", "englanti", "ruotsi"};

    // JSONin tulostus
    std::cout << j.dump(2) << std::endl;
    
    // JSONista datan lukeminen
    std::string nimi = j["nimi"];
    int ikä = j["ikä"];
    std::cout << "Nimi: " << nimi << ", Ikä: " << ikä << std::endl;

    return 0;
}
```
Output:
```json
{
  "ikä": 30,
  "kielitaito": [
    "suomi",
    "englanti",
    "ruotsi"
  ],
  "nimi": "Maija"
}
Nimi: Maija, Ikä: 30
```

## Syvemmälle:
JSON, lyhenne Java Script Object Notation, kehitettiin alun perin JavaScriptille, mutta on nykyään kieliriippumaton. XML oli ennen JSONin suosion kasvua yleisin valinta datan välittämiseen, mutta sen raskas syntaksi on johtanut monien siirtymiseen käyttämään JSONia. JSONia käsitteleviä kirjastoja on monia, tässä artikkelissa käytetty `nlohmann/json` on yksi suosituimmista C++-kielellä.

## Katso Myös:
- JSON-standardi: https://www.json.org/json-fi.html
- nlohmann/json GitHub-sivu: https://github.com/nlohmann/json
- C++ JSON-kirjastojen vertailu: https://en.cppreference.com/w/cpp/links/libs/json
