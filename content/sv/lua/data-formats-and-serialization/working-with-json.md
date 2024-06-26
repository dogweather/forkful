---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:28.687588-07:00
description: "Hur man g\xF6r: Lua inkluderar inte ett inbyggt bibliotek f\xF6r JSON-bearbetning.\
  \ D\xE4rf\xF6r \xE4r ett av de popul\xE4ra tredjepartsbiblioteken `dkjson`, som\
  \ du enkelt\u2026"
lastmod: '2024-03-13T22:44:38.060162-06:00'
model: gpt-4-0125-preview
summary: "Lua inkluderar inte ett inbyggt bibliotek f\xF6r JSON-bearbetning."
title: Arbeta med JSON
weight: 38
---

## Hur man gör:
Lua inkluderar inte ett inbyggt bibliotek för JSON-bearbetning. Därför är ett av de populära tredjepartsbiblioteken `dkjson`, som du enkelt kan använda för JSON-kodning och avkodning. Först, se till att installera `dkjson`, t.ex., genom LuaRocks (`luarocks install dkjson`), och följ sedan exemplen nedan.

### Avkodning av JSON till Lua-tabell
```lua
local dkjson = require "dkjson"

local jsonString = '{"name": "Lua Programmer", "age": 30, "languages": ["Lua", "JavaScript"]}'
local luaTable, pos, err = dkjson.decode(jsonString, 1, nil)
if err then
  print ("Fel:", err)
else
  print("Namn:", luaTable.name) -- Utmatning: Namn: Lua Programmer
  print("Ålder:", luaTable.age) -- Utmatning: Ålder: 30
  print("Språk:", table.concat(luaTable.languages, ", ")) -- Utmatning: Språk: Lua, JavaScript
end
```

### Kodning av Lua-tabell till JSON
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

Exempelutmatning för kodning:
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

Dessa enkla exempel visar hur man arbetar med JSON i Lua, vilket gör det enkelt att integrera Lua-applikationer med olika webbteknologier och externa API:er. Kom ihåg, även om `dkjson` används i dessa exempel, kan andra bibliotek som `cjson` och `RapidJSON` också vara lämpliga alternativ beroende på ditt projekts behov.
