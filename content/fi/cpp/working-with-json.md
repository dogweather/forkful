---
title:                "C++: Työskentely jsonin kanssa"
simple_title:         "Työskentely jsonin kanssa"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/working-with-json.md"
---

{{< edit_this_page >}}

## Miksi
JSON (JavaScript Object Notation) on datan tallennusmuoto, joka on saavuttanut suuren suosion ohjelmoinnissa sen yksinkertaisuuden ja monikäyttöisyyden ansiosta. JSONia käytetään usein tiedonsiirrossa ja sillä on laaja tuki eri ohjelmointikielillä, kuten C++.

## Kuinka
JSON:n käsittely C++:ssa on helppoa ja suoraviivaista. Seuraavassa on esimerkki kuinka voit lukea JSON-tiedoston ja hakea siitä tiettyjä arvoja:

```C++
#include <iostream>
#include <fstream>
#include <nlohmann/json.hpp> // JSON-kirjasto

int main() {
    // Avataan tiedosto
    std::ifstream file("data.json");
    if (!file.is_open()) {
        std::cout << "Tiedoston avaaminen epäonnistui." << std::endl;
        return 1;
    }

    // Luetaan tiedosto JSON-oliona
    nlohmann::json data;
    file >> data;

    // Tulostetaan tietueiden nimet
    std::cout << "Nimet:" << std::endl;
    for (auto& tietue : data) {
        std::cout << tietue["nimi"] << std::endl;
    }

    // Suljetaan tiedosto
    file.close();

    return 0;
}
```

Yllä oleva koodi lukee tiedoston nimeltä "data.json" ja tulostaa siitä löytyvien tietueiden nimet. Tietueiden päälle pääsee käsiksi hakasulkeilla ja niiden sisältöä voi tulostaa esimerkiksi käyttämällä "cout" -operaattoria.

JSON-tiedosto voisi näyttää tältä:

```json
[
    {
        "nimi": "Matti",
        "ika": 23,
        "sijainti": "Helsinki"
    },
    {
        "nimi": "Anna",
        "ika": 27,
        "sijainti": "Tampere"
    }
]
```

Ja ohjelman tulostus olisi seuraava:

```
Nimet:
"Matti"
"Anna"
```

## Syvempi sukellus
JSON-tietueet ovat oikeastaan vain avain-arvo -pareja. Avaimet ovat merkkijonoja ja arvot voivat olla esimerkiksi lukuja, merkkijonoja tai jopa toisia tietueita. Tiedostoa luettaessa avaimilla pääsee käsiksi haluttuihin arvoihin, kuten edellisessä esimerkissä tehtiin.

JSON-kirjastoja löytyy useita ja niiden käyttö voi vaihdella hieman. Kannattaa aina tutustua käytetyn kirjaston dokumentaatioon ja esimerkkeihin tarkemmin.

## Katso myös
- JSON-dokumentointi (https://www.json.org/)
- JSON-kirjasto C++:lle (https://github.com/nlohmann/json)
- JSON-esimerkkejä (https://www.w3schools.com/js/js_json_intro.asp)