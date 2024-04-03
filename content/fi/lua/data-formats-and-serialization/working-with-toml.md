---
date: 2024-01-26 04:24:12.699271-07:00
description: "Kuinka: Ensiksi, varmista ett\xE4 Lua-ymp\xE4rist\xF6ss\xE4si on TOML-j\xE4\
  sennin. T\xE4ss\xE4 esimerkiss\xE4 k\xE4yt\xE4mme `lua-toml`:ia."
lastmod: '2024-03-13T22:44:56.719938-06:00'
model: gpt-4-0125-preview
summary: "Ensiksi, varmista ett\xE4 Lua-ymp\xE4rist\xF6ss\xE4si on TOML-j\xE4sennin."
title: "Ty\xF6skentely TOML:n kanssa"
weight: 39
---

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
