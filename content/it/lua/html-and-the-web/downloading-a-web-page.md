---
date: 2024-01-20 17:44:15.122165-07:00
description: 'How to: Dopo aver eseguito il codice, vedrai il contenuto HTML della
  pagina `http://www.esempio.com` stampato sullo schermo.'
lastmod: '2024-04-05T21:53:44.320358-06:00'
model: gpt-4-1106-preview
summary: Dopo aver eseguito il codice, vedrai il contenuto HTML della pagina `http://www.esempio.com`
  stampato sullo schermo.
title: Scaricare una pagina web
weight: 42
---

## How to:
```Lua
local http = require("socket.http")
local ltn12 = require("ltn12")

local function scarica_pagina(url)
    local response_body = {}
    http.request{
        url = url,
        sink = ltn12.sink.table(response_body)
    }
    return table.concat(response_body)
end

local url = "http://www.esempio.com"
local content = scarica_pagina(url)
print(content)
```

Dopo aver eseguito il codice, vedrai il contenuto HTML della pagina `http://www.esempio.com` stampato sullo schermo.

## Deep Dive
Lua non ha una libreria HTTP standard inclusa come altri linguaggi. Pertanto, dobbiamo usare `LuaSocket`, una libreria esterna per le operazioni di rete. Prima di Lua 5.1, i programmatori dovevano implementare tale funzionalità da zero o usare binari non affidabili. Alternativamente, puoi anche usare `luasec` per HTTPS, ma è un po' più complesso.

## See Also
- LuaSocket documentation: http://w3.impa.br/~diego/software/luasocket
- LuaSec per la navigazione sicura HTTPS: https://github.com/brunoos/luasec/wiki
- Tutorial Lua: https://www.lua.org/pil/contents.html
