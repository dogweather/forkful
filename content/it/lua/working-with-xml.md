---
title:                "Lavorare con XML"
date:                  2024-01-26T04:33:29.570149-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con XML"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/working-with-xml.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?
Lavorare con XML implica l'analisi e la manipolazione di documenti XML tramite codice. I programmatori lo fanno per leggere, scrivere e modificare dati in un formato strutturato e portatile, ampiamente utilizzato per lo scambio e l'archiviazione dei dati.

## Come fare:
Lua non include un'analisi XML nativa, ma ci sono librerie come LuaXML e xml2lua che svolgono il lavoro. Ecco una rapida occhiata all'analisi di XML con xml2lua:

```Lua
local xml2lua = require("xml2lua")
local handler = require("xmlhandler.tree")

local xmlParser = xml2lua.parser(handler)
xmlParser:parse([[<root><book id="123">Programmazione in Lua</book></root>]])

print(handler.root.book._attr.id)  -- Risultati: 123
print(handler.root.book[1])        -- Risultati: Programmazione in Lua
```

Per scrivere XML, ecco un mini esempio utilizzando LuaXML:

```Lua
local luaxml = require("LuaXML")

local xml = xml.new("root")
xml:append("book")[1] = "Programmazione in Lua"
xml.book._attr = {id="123"}

print(xml:tag())  -- Risultati: <root><book id="123">Programmazione in Lua</book></root>
```

## Approfondimento
XML, acronimo di Extensible Markup Language, è uno standard nella rappresentazione e scambio di dati dalla metà degli anni '90. Dà struttura ai dati ed è sia leggibile dall'uomo che analizzabile dalla macchina.

Mentre JSON e YAML sono ora favoriti per la loro semplicità, XML rimane prevalente in molti sistemi aziendali e legacy. In Lua, la gestione nativa dell'XML non è incorporata perché Lua è progettata per essere piccola ed estensibile tramite moduli.

Le librerie XML per Lua, come LuaXML, xml2lua e altre, colmano questa lacuna. LuaXML offre un lettore e scrittore XML leggero, mentre xml2lua utilizza un approccio basato sugli eventi simile ai parser SAX. Queste librerie sono di solito implementate in Lua puro per portabilità, mentre alcune potrebbero fare affidamento su C per le prestazioni.

Quando si tratta di prestazioni e uso della memoria, le librerie XML di Lua potrebbero non essere veloci come quelle in linguaggi con supporto nativo. Tuttavia, per la maggior parte dei casi d'uso in Lua, specialmente nello sviluppo di giochi o nella scrittura di script per sistemi embedded, queste librerie fanno un ottimo lavoro senza sovraccaricare il sistema.

## Vedi Anche
- LuaXML su GitHub: https://github.com/LuaDist/luaxml
- xml2lua su GitHub: https://github.com/manoelcampos/xml2lua
- Lista di librerie di Lua.org: https://lua-users.org/wiki/LibrariesAndBindings