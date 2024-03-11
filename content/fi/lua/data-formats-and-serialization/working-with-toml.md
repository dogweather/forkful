---
date: 2024-01-26 04:24:12.699271-07:00
description: "TOML:n kanssa ty\xF6skentely sis\xE4lt\xE4\xE4 TOML-tietojen (Tom\u2019\
  s Obvious, Minimal Language) j\xE4sent\xE4misen ja tuottamisen Lualla. Ohjelmoijat\
  \ k\xE4ytt\xE4v\xE4t TOML:\xE4\xE4\u2026"
lastmod: '2024-03-11T00:14:30.673706-06:00'
model: gpt-4-0125-preview
summary: "TOML:n kanssa ty\xF6skentely sis\xE4lt\xE4\xE4 TOML-tietojen (Tom\u2019\
  s Obvious, Minimal Language) j\xE4sent\xE4misen ja tuottamisen Lualla. Ohjelmoijat\
  \ k\xE4ytt\xE4v\xE4t TOML:\xE4\xE4\u2026"
title: "Ty\xF6skentely TOML:n kanssa"
---

{{< edit_this_page >}}

## Mikä & Miksi?
TOML:n kanssa työskentely sisältää TOML-tietojen (Tom’s Obvious, Minimal Language) jäsentämisen ja tuottamisen Lualla. Ohjelmoijat käyttävät TOML:ää konfiguraatiotiedostoihin sen luettavuuden ja yksinkertaisen syntaksin vuoksi, joka kääntyy helposti datarakenteeksi.

## Kuinka:
Ensiksi, varmista että Lua-ympäristössäsi on TOML-jäsennin. Tässä esimerkissä käytämme `lua-toml`:ia.

```Lua
local toml = require("toml")

-- Jäsennä TOML-merkkijono
local toml_data = [[
title = "TOML Esimerkki"

[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
]]

local data = toml.parse(toml_data)
print(data.title) -- "TOML Esimerkki"

-- Tuota TOML-merkkijono
local table_data = {
  title = "TOML Esimerkki",
  owner = {
    name = "Tom Preston-Werner",
    dob = os.time({year=1979, month=5, day=27, hour=7, min=32})
  }
}

local toml_string = toml.encode(table_data)
print(toml_string)
```

Esimerkkituloste:
```
TOML Esimerkki
```

## Syvempi sukellus
TOML:n loi Tom Preston-Werner vuonna 2013 vaihtoehdoksi muille datan sarjallistamiskielille kuten XML ja YAML, tarjoten suoraviivaisemman muodon konfiguraatiodatan esittämiseen. Vaikka JSON on kaikkialla, sen syntaksi voi olla hankala konfiguraatiotiedostoille. TOML erottuu selkeämmällä syntaksilla ihmisille, muistuttaen .ini-tiedostoja, mutta sisältäen peseytymiskyvyt ja datatyypit.

Vaihtoehtoja TOML:lle ovat JSON, YAML ja XML. Kuitenkin TOML on erityisesti suunniteltu konfiguraatioon ja on argumentoitavasti yksinkertaisempi kuin YAML, luettavampi kuin JSON konfiguraatiotarkoituksiin ja vähemmän monisanainen kuin XML.

TOML-käsittelyn toteuttaminen Luassa yleensä vaatii kolmannen osapuolen kirjaston. Suorituskyky ja ominaisuudet voivat vaihdella perusjäsennyksestä täydelliseen sarjallistamistukeen. Käsitellessä suuria konfiguraatiotiedostoja tai usein toistuvia luku-/kirjoitusoperaatioita, harkitse kirjaston suorituskykyä ja yhteensopivuutta viimeisimmän TOML-version kanssa.

## Katso myös
- TOML-spesifikaatio: https://toml.io/en/
- `lua-toml` kirjasto: https://github.com/jonstoler/lua-toml
- Datan sarjallistamismuotojen vertailu: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
