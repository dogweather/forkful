---
title:                "Työskentely TOML:n kanssa"
date:                  2024-01-26T04:19:48.386676-07:00
model:                 gpt-4-0125-preview
simple_title:         "Työskentely TOML:n kanssa"

category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/working-with-toml.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
TOML (Tom's Obvious, Minimal Language) on datan serialisointiformaatti, joka on helppo lukea sen selkeän semantiikan ansiosta. Ohjelmoijat käyttävät TOMLia konfiguraatiotiedostoissa, koska se tarjoaa tasapainon ihmisen luettavuuden ja koneen jäsentämisen välillä.

## Kuinka:
Työskennellessäsi TOMLin kanssa C++:ssa, tarvitset kirjaston kuten `toml++`. Tässä nopea aloitus:

```C++
#include <toml++/toml.h>
#include <iostream>
#include <fstream>

int main() {
    // Jäsentää TOMLin tiedostosta
    std::ifstream ifs("config.toml");
    auto config = toml::parse(ifs);

    // Arvon käyttäminen
    std::string title = config["title"].value_or("Nimeämätön");
    std::cout << "Otsikko: " << title << '\n';

    // Muokkaa ja tallenna TOML
    config["title"] = "Uusi Otsikko";
    std::ofstream ofs("config.toml");
    ofs << config;
}
```

Esimerkki `config.toml`:
```toml
title = "Esimerkki"
```

Esimerkkituloste:
```plaintext
Otsikko: Esimerkki
```

## Syväsukellus
TOMLin loi Tom Preston-Werner vuonna 2013 vaihtoehtona YAMLille ja JSONille. Se on suunniteltu yksinkertaiseksi ja eksplisiittiseksi, pääasiassa konfiguraatiotiedostoja varten. Toisin kuin JSON, TOML keskittyy olemaan epäselvyydetön, mikä tarkoittaa, että asiakirjan jäsentäminen on determinististä.

Vaihtoehtoja TOMLille ovat YAML, joka on sallivampi siinä, mitä se sallii, joskus kuitenkin ennustettavuuden kustannuksella. Toinen vaihtoehto, JSON, on rakenteeltaan melko tiukka, mutta ei yhtä ihmisystävällinen konfiguraatioissa kommenttien puutteen ja aaltosulkujen intensiivisen syntaksin vuoksi.

Toteutuksessa `toml++` on header-only C++17 kirjasto, joka noudattaa uusinta TOML-spesifikaatiota. Se tarjoaa DOM-tyyppisen käyttöliittymän TOML-datan navigointiin ja manipulointiin, mikä tekee siitä suoraviivaista integroida projekteihin. Kirjasto huolehtii jäsentämisestä, validoinnista ja tulostuksen generoinnista, mahdollistaen sinun hakea ja asettaa TOML-dataa käyttäen C++ tyyppejä.

## Katso Myös
- TOML GitHub varasto: https://github.com/toml-lang/toml
- `toml++`, C++ kirjasto TOMLille: https://github.com/marzer/tomlplusplus
- Virallinen TOML dokumentaatio, jossa yksityiskohtaiset selitykset formaatista: https://toml.io/en/
