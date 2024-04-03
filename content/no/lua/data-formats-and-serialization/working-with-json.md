---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:35.216372-07:00
description: "Hvordan: Lua inkluderer ikke et innebygd bibliotek for JSON-behandling.\
  \ Derfor er et av de popul\xE6re tredjepartsbibliotekene `dkjson`, som du enkelt\
  \ kan\u2026"
lastmod: '2024-03-13T22:44:40.951736-06:00'
model: gpt-4-0125-preview
summary: Lua inkluderer ikke et innebygd bibliotek for JSON-behandling.
title: Arbeider med JSON
weight: 38
---

## Hvordan:
Lua inkluderer ikke et innebygd bibliotek for JSON-behandling. Derfor er et av de populære tredjepartsbibliotekene `dkjson`, som du enkelt kan bruke for JSON-koding og dekoding. Først, sørg for å installere `dkjson`, f.eks., gjennom LuaRocks (`luarocks install dkjson`), og følg deretter eksemplene nedenfor.

### Dekoding av JSON til Lua-tabell
```lua
local dkjson = require "dkjson"

local jsonString = '{"name": "Lua Programmer", "age": 30, "languages": ["Lua", "JavaScript"]}'
local luaTable, pos, err = dkjson.decode(jsonString, 1, nil)
if err then
  print ("Feil:", err)
else
  print("Navn:", luaTable.name) -- Utdata: Navn: Lua Programmer
  print("Alder:", luaTable.age) -- Utdata: Alder: 30
  print("Språk:", table.concat(luaTable.languages, ", ")) -- Utdata: Språk: Lua, JavaScript
end
```

### Koding av Lua-tabell til JSON
```lua
local dkjson = require "dkjson"

local luaTable = {
  name = "Lua Programmer",
  age = 30,
  languages = { "Lua", "JavaScript" }
}

local jsonString = dkjson.encode(luaTable, { indent = true })
print(jsonString)
```

Eksempelutdata for koding:
```json
{
  "age": 30,
  "languages": [
    "Lua",
    "JavaScript"
  ],
  "name": "Lua Programmer"
}
```

Disse enkle eksemplene demonstrerer hvordan man arbeider med JSON i Lua, noe som gjør det enkelt å integrere Lua-applikasjoner med ulike webteknologier og eksterne APIer. Husk, mens `dkjson` brukes i disse eksemplene, kan andre biblioteker som `cjson` og `RapidJSON` også være passende alternativer avhengig av prosjektets behov.
