---
title:                "Lavorare con JSON"
aliases:
- /it/lua/working-with-json.md
date:                  2024-02-03T19:23:14.389597-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa & Perché?
Lavorare con JSON in Lua comporta l'analisi di stringhe in formato JSON per trasformarle in tabelle Lua e viceversa, consentendo un facile scambio di dati tra applicazioni Lua e servizi web o API esterne. I programmatori lo fanno per sfruttare il formato leggero e facile da analizzare di JSON per un'efficiente memorizzazione dei dati, configurazione o comunicazione API.

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
