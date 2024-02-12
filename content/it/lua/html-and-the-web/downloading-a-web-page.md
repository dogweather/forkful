---
title:                "Scaricare una pagina web"
aliases: - /it/lua/downloading-a-web-page.md
date:                  2024-01-20T17:44:15.122165-07:00
model:                 gpt-4-1106-preview
simple_title:         "Scaricare una pagina web"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? 
Scaricare una pagina web significa prelevare il suo contenuto HTML da internet. I programmatori lo fanno per analizzare dati, integrare funzionalità e automatizzare test sulle app web.

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
