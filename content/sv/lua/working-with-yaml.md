---
title:                "Arbete med YAML"
date:                  2024-01-19
html_title:           "Arduino: Arbete med YAML"
simple_title:         "Arbete med YAML"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/working-with-yaml.md"
---

{{< edit_this_page >}}

## Vad & Varför?
YAML är ett dataformat lätt för människor att läsa och skriva, ofta använd för konfigurationsfiler. Programmerare använder det för dess klarhet och portabilitet mellan olika språk och verktyg.

## Hur gör man:
För att hantera YAML i Lua behöver du ett bibliotek som `lyaml`. Installera det via luarocks:

```bash
luarocks install lyaml
```

Använd biblioteket för att tolka en YAML-sträng:

```Lua
local lyaml = require('lyaml')

local yaml_data = [[
- Husdjur: Katt
  Ålder: 3
- Husdjur: Hund
  Ålder: 5
]]

local table_from_yaml = lyaml.load(yaml_data)
for _, pet in ipairs(table_from_yaml) do
  print(pet.Husdjur .. " är " .. pet.Ålder .. " år gammal.")
end
```

Exempel på utskrift:

```text
Katt är 3 år gammal.
Hund är 5 år gammal.
```

## På djupet
YAML, som står för "YAML Ain't Markup Language", introducerades i början av 2000-talet som ett enklare alternativ till XML och JSON. Alternativ inkluderar just dessa, samt TOML. Lua stöder inte YAML direkt, men bibliotek som `lyaml` ger tillgång till parsing och generering.

## Se även
- YAML officiell webbsida: [yaml.org](https://yaml.org)
- `lyaml` biblioteket på Github: [github.com/gvvaughan/lyaml](https://github.com/gvvaughan/lyaml)
- LuaRocks: [luarocks.org](https://luarocks.org/)
