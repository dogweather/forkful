---
title:                "Lavorare con JSON"
date:                  2024-01-19
html_title:           "Arduino: Lavorare con JSON"
simple_title:         "Lavorare con JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/working-with-json.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?
JSON (JavaScript Object Notation) è un formato per lo scambio di dati. I programmatori lo usano per la sua semplicità e leggibilità tra sistemi diversi.

## Come fare:
Per lavorare con JSON in Lua, avrai bisogno di un modulo per fare il parsing e serializzare; `dkjson` è un'ottima scelta. Ecco un esempio:

```Lua
local dkjson = require "dkjson"

-- Serializzazione: tabella Lua a stringa JSON
local tabella = { nome = "Luigi", eta = 30, linguaggi = {"Lua", "C++"} }
local str_json = dkjson.encode(tabella)
print(str_json)

-- Parsing: stringa JSON a tabella Lua
local str_json = '{"nome":"Luigi","eta":30,"linguaggi":["Lua","C++"]}'
local tabella, pos, err = dkjson.decode(str_json, 1, nil)
if err then
  error(err)
else
  for k,v in pairs(tabella) do
    print(k,v)
  end
end
```

Output:
```
{"nome":"Luigi","eta":30,"linguaggi":["Lua","C++"]}
nome    Luigi
eta     30
linguaggi   table: 0x7f9f50c0a600
```

## Approfondimenti
JSON nasce nel 2001 da Douglas Crockford. In Lua, non è integrato nativamente come in JavaScript, quindi si usano moduli esterni. `dkjson` è affidabile ma esistono alternative come `cjson` e `Luajson`. L'implementazione prevede il parsing di stringhe in tabelle Lua e viceversa attraverso l'uso di funzioni come `encode` e `decode`.

## Vedi Anche
- La documentazione ufficiale di `dkjson`: http://dkolf.de/src/dkjson-lua.fsl/home
- JSON.org per capire meglio il formato JSON: https://www.json.org/json-en.html
- `cjson` modulo Lua per JSON: https://www.kyne.com.au/~mark/software/lua-cjson-manual.html
- `Luajson` su GitHub: https://github.com/harningt/luajson
