---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:54.389349-07:00
description: 'Hoe te: Zorg eerst dat je Lua-omgeving een TOML-parser heeft. We gebruiken
  `lua-toml` voor dit voorbeeld.'
lastmod: '2024-03-13T22:44:50.960341-06:00'
model: gpt-4-0125-preview
summary: Zorg eerst dat je Lua-omgeving een TOML-parser heeft.
title: Werken met TOML
weight: 39
---

## Hoe te:
Zorg eerst dat je Lua-omgeving een TOML-parser heeft. We gebruiken `lua-toml` voor dit voorbeeld.

```Lua
local toml = require("toml")

-- Parse TOML string
local toml_data = [[
title = "TOML Voorbeeld"

[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
]]

local data = toml.parse(toml_data)
print(data.title) -- "TOML Voorbeeld"

-- Genereer TOML string
local table_data = {
  title = "TOML Voorbeeld",
  owner = {
    name = "Tom Preston-Werner",
    dob = os.time({year=1979, month=5, day=27, hour=7, min=32})
  }
}

local toml_string = toml.encode(table_data)
print(toml_string)
```

Voorbeelduitvoer:
```
TOML Voorbeeld
```

## Diepere Duik
TOML is in 2013 gecreëerd door Tom Preston-Werner als alternatief voor andere data serialisatie talen zoals XML en YAML, biedend een eenvoudiger formaat om configuratiegegevens te vertegenwoordigen. Hoewel JSON alomtegenwoordig is, kan de syntax ervan omslachtig zijn voor configuratiebestanden. TOML blinkt uit met een duidelijkere syntax voor mensen, gelijkend op .ini-bestanden maar met mogelijkheden voor nesten en datatypes.

Alternatieven voor TOML zijn onder andere JSON, YAML en XML. Echter, TOML is specifiek ontworpen voor configuratie en is naar mening eenvoudiger dan YAML, leesbaarder dan JSON voor configuratiedoeleinden, en minder langdradig dan XML.

De implementatie van TOML-afhandeling in Lua vereist over het algemeen een bibliotheek van derden. Prestaties en functies kunnen variëren, van basis parsing tot volledige serialisatieondersteuning. Bij het omgaan met grote configuratiebestanden of regelmatige lees-/schrijfbewerkingen, overweeg dan de prestaties van de bibliotheek en de naleving van de laatste TOML-versie.

## Zie Ook
- TOML Specificatie: https://toml.io/nl/
- `lua-toml` bibliotheek: https://github.com/jonstoler/lua-toml
- Vergelijking van Data Serialisatieformaten: https://nl.wikipedia.org/wiki/Vergelijking_van_data-serialisatieformaten
