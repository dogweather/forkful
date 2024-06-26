---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:34.984319-07:00
description: "Come fare: Lua non ha una libreria incorporata per l'analisi dell'HTML,\
  \ ma \xE8 possibile utilizzare librerie di terze parti come `LuaHTML` o sfruttare\
  \ i\u2026"
lastmod: '2024-03-13T22:44:43.555686-06:00'
model: gpt-4-0125-preview
summary: "Lua non ha una libreria incorporata per l'analisi dell'HTML, ma \xE8 possibile\
  \ utilizzare librerie di terze parti come `LuaHTML` o sfruttare i binding per `libxml2`\
  \ tramite `LuaXML`."
title: Analisi del HTML
weight: 43
---

## Come fare:
Lua non ha una libreria incorporata per l'analisi dell'HTML, ma è possibile utilizzare librerie di terze parti come `LuaHTML` o sfruttare i binding per `libxml2` tramite `LuaXML`. Un approccio popolare è utilizzare la libreria `lua-gumbo` per l'analisi dell'HTML, che fornisce una capacità di parsing conforme ad HTML5 e diretta.

### Installazione di lua-gumbo:
Prima di tutto, assicurati che `lua-gumbo` sia installato. Tipicamente puoi installarlo usando luarocks:

```sh
luarocks install lua-gumbo
```

### Parsing di base con lua-gumbo:
Ecco come puoi analizzare un frammento HTML semplice ed estrarre dati da esso usando `lua-gumbo`:

```lua
local gumbo = require "gumbo"
local document = gumbo.parse[[<html><body><p>Ciao, mondo!</p></body></html>]]

local p = document:getElementsByTagName("p")[1]
print(p.textContent)  -- Output: Ciao, mondo!
```

### Esempio Avanzato - Estrazione di Link:
Per estrarre gli attributi `href` da tutti i tag di ancoraggio (`<a>` elements) in un documento HTML:

```lua
local gumbo = require "gumbo"
local document = gumbo.parse([[
<html>
<head><title>Pagina Campione</title></head>
<body>
  <a href="http://example.com/1">Link 1</a>
  <a href="http://example.com/2">Link 2</a>
  <a href="http://example.com/3">Link 3</a>
</body>
</html>
]])

for _, element in ipairs(document.links) do
    if element.getAttribute then  -- Assicurati che sia un Elemento e che abbia attributi
        local href = element:getAttribute("href")
        if href then print(href) end
    end
end

-- Esempio di Output:
-- http://example.com/1
-- http://example.com/2
-- http://example.com/3
```

Questo frammento di codice itera attraverso tutti i link nel documento e stampa i loro attributi `href`. La capacità della libreria `lua-gumbo` di analizzare e comprendere la struttura di un documento HTML semplifica il processo di estrazione di elementi specifici in base ai loro tag o attributi.
