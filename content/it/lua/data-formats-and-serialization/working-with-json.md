---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:14.389597-07:00
description: "Come fare: Lua non include una libreria integrata per l'elaborazione\
  \ di JSON. Pertanto, una delle librerie di terze parti pi\xF9 popolari \xE8 `dkjson`,\
  \ che\u2026"
lastmod: '2024-03-13T22:44:43.578372-06:00'
model: gpt-4-0125-preview
summary: Lua non include una libreria integrata per l'elaborazione di JSON.
title: Lavorare con JSON
weight: 38
---

## Come fare:
Lua non include una libreria integrata per l'elaborazione di JSON. Pertanto, una delle librerie di terze parti più popolari è `dkjson`, che puoi facilmente utilizzare per la codifica e la decodifica di JSON. Prima cosa, assicurati di installare `dkjson`, ad esempio, tramite LuaRocks (`luarocks install dkjson`), e poi segui gli esempi qui sotto.

### Decodifica di JSON in Tabella Lua
```lua
local dkjson = require "dkjson"

local jsonString = '{"name": "Programmatore Lua", "age": 30, "languages": ["Lua", "JavaScript"]}'
local luaTable, pos, err = dkjson.decode(jsonString, 1, nil)
if err then
  print ("Errore:", err)
else
  print("Nome:", luaTable.name) -- Output: Nome: Programmatore Lua
  print("Età:", luaTable.age) -- Output: Età: 30
  print("Linguaggi:", table.concat(luaTable.languages, ", ")) -- Output: Linguaggi: Lua, JavaScript
end
```

### Codifica di Tabella Lua in JSON
```lua
local dkjson = require "dkjson"

local luaTable = {
  name = "Programmatore Lua",
  age = 30,
  languages = { "Lua", "JavaScript" }
}

local jsonString = dkjson.encode(luaTable, { indent = true })
print(jsonString)
```

Esempio di output per la codifica:
```json
{
  "age": 30,
  "languages": [
    "Lua",
    "JavaScript"
  ],
  "name": "Programmatore Lua"
}
```

Questi semplici esempi mostrano come lavorare con JSON in Lua, rendendo facile integrare applicazioni Lua con varie tecnologie web e API esterne. Ricorda, mentre `dkjson` è utilizzato in questi esempi, altre librerie come `cjson` e `RapidJSON` possono anche essere alternative adatte a seconda delle esigenze del tuo progetto.
