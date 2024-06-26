---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:26.897980-07:00
description: "Kuinka: Lua ei sis\xE4ll\xE4 valmiiksi tehty\xE4 kirjastoa JSON-k\xE4\
  sittelyyn. Siksi yhten\xE4 suosittuna kolmannen osapuolen kirjastona on `dkjson`,\
  \ jota voit helposti\u2026"
lastmod: '2024-03-13T22:44:56.717862-06:00'
model: gpt-4-0125-preview
summary: "Lua ei sis\xE4ll\xE4 valmiiksi tehty\xE4 kirjastoa JSON-k\xE4sittelyyn."
title: "Ty\xF6skentely JSON:n kanssa"
weight: 38
---

## Kuinka:
Lua ei sisällä valmiiksi tehtyä kirjastoa JSON-käsittelyyn. Siksi yhtenä suosittuna kolmannen osapuolen kirjastona on `dkjson`, jota voit helposti käyttää JSONin koodaamiseen ja dekoodaamiseen. Varmista ensin, että olet asentanut `dkjson`in, esim. käyttäen LuaRocksia (`luarocks install dkjson`), ja sitten seuraa alla olevia esimerkkejä.

### JSONin dekoodaus Lua-taulukoksi
```lua
local dkjson = require "dkjson"

local jsonString = '{"name": "Lua Programmer", "age": 30, "languages": ["Lua", "JavaScript"]}'
local luaTable, pos, err = dkjson.decode(jsonString, 1, nil)
if err then
  print ("Error:", err)
else
  print("Nimi:", luaTable.name) -- Tuloste: Nimi: Lua Programmer
  print("Ikä:", luaTable.age) -- Tuloste: Ikä: 30
  print("Kielet:", table.concat(luaTable.languages, ", ")) -- Tuloste: Kielet: Lua, JavaScript
end
```

### Lua-taulukon koodaus JSONiksi
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

Esimerkkituloste koodauksesta:
```json
{
  "ikä": 30,
  "kielet": [
    "Lua",
    "JavaScript"
  ],
  "nimi": "Lua Programmer"
}
```

Nämä yksinkertaiset esimerkit osoittavat, kuinka työskennellä JSONin kanssa Luassa, tehdäkseen Lualla tehtyjen sovellusten integraation erilaisten web-teknologioiden ja ulkoisten APIen kanssa helpoksi. Muista, että vaikka näissä esimerkeissä käytetään `dkjson`ia, myös muut kirjastot kuten `cjson` ja `RapidJSON` voivat olla sopivia vaihtoehtoja projektisi tarpeista riippuen.
