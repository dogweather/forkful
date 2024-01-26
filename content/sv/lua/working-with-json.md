---
title:                "Arbeta med JSON"
html_title:           "Arduino: Arbeta med JSON"
simple_title:         "Arbeta med JSON"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/working-with-json.md"
---

{{< edit_this_page >}}

## Vad & Varför?
JSON hanterar data som lättlästa textobjekt, perfekt för datadelning mellan servrar och webbklienter. Programmerare använder det för dess enkelhet och språkoberoende format.

## Hur gör man:
I Lua använder vi ofta ett bibliotek som `dkjson` eller `cjson` för att hantera JSON. Här kommer exempel på hur man använder `dkjson`:

```Lua
local dkjson = require "dkjson"

-- Konvertera Lua-tabell till JSON
local minTabell = { namn = "Erik", ålder = 30, intressen = {"programmering", "schack"} }
local jsonStr = dkjson.encode(minTabell)
print(jsonStr) -- Output: {"intressen":["programmering","schack"],"namn":"Erik","ålder":30}

-- Konvertera JSON till Lua-tabell
local jsonStr = '{"namn":"Erik","ålder":30,"intressen":["programmering","schack"]}'
local tabell, pos, err = dkjson.decode(jsonStr, 1, nil)
if err then
  print("Fel: " .. err)
else
  print(tabell.namn) -- Output: Erik
end
```

## Djupdykning
JSON (JavaScript Object Notation) utvecklades ur JavaScript men har blivit språkoberoende. XML var tidigare standard för datadelning men JSON tog över tack vare sin kompakthet och hastighet. `dkjson` är skriven i ren Lua och är portabel, medan `cjson` använder C för prestanda. Att välja bibliotek beror på dina behov – prestanda kontra portabilitet.

## Se också
- Lua `dkjson` modul: http://dkolf.de/src/dkjson-lua.fsl/
- Online JSON validator och formatter: https://jsonlint.com/
- JSON officiell webbplats: https://www.json.org/json-sv.html
