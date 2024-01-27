---
title:                "JSON-tiedostojen käsittely"
date:                  2024-01-19
html_title:           "Arduino: JSON-tiedostojen käsittely"
simple_title:         "JSON-tiedostojen käsittely"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON (JavaScript Object Notation) on kevyt tiedonvaihtoformaatti. Ohjelmoijat käyttävät JSONia sen yksinkertaisuuden ja ihmisen sekä koneen luettavuuden vuoksi.

## How to:
```Lua
-- Ladataan JSON-kirjasto
local json = require("dkjson")

-- JSON-muotoisen merkkijonon käsittely
local jsonString = '{"nimi": "Matti", "ika": 30}'
local luaTable = json.decode(jsonString)

print(luaTable.nimi)  -- Output: Matti

-- Luodaan taulukko ja muunnetaan JSONiksi
local uusiTable = { nimi = "Liisa", ika = 28 }
local uusiJsonString = json.encode(uusiTable)

print(uusiJsonString)  -- Output: {"ika":28,"nimi":"Liisa"}
```

## Deep Dive
JSON syntyi 2000-luvun alussa, helpottamaan JavaScriptin ja palvelimien välisiä tietojenvaihtoja. XML oli ennen JSONia suosittu, mutta JSONin keveys ja nopeus ovat sen valttikortteja. Lua ei sisällä vakio JSON-tukea, joten ulkopuolisia kirjastoja, kuten 'dkjson', käytetään JSONin käsittelyyn.

## See Also
- Lua JSON kirjaston kotisivu: http://dkolf.de/src/dkjson-lua.fsl/home
- JSON viralliset kotisivut: https://www.json.org/json-fi.html
- Online JSON visualisointi: https://jsoneditoronline.org/
