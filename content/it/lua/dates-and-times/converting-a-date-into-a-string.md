---
date: 2024-01-20 17:36:54.632144-07:00
description: "How to: In Lua, convertire una data in stringa \xE8 semplice. Ecco un\
  \ esempio con `os.date`."
lastmod: '2024-03-13T22:44:43.568879-06:00'
model: gpt-4-1106-preview
summary: "In Lua, convertire una data in stringa \xE8 semplice."
title: Conversione di una data in una stringa
weight: 28
---

## How to:
In Lua, convertire una data in stringa è semplice. Ecco un esempio con `os.date`.

```Lua
-- Ottenere la data e ora attuale
local timestamp = os.time()
local formatted_date = os.date("%Y-%m-%d %H:%M:%S", timestamp)

print(formatted_date) -- Output: 2023-04-01 15:24:36
```

Vuoi solo la data? Nessun problema:

```Lua
local only_date = os.date("%Y-%m-%d")
print(only_date) -- Output: 2023-04-01
```

E se hai bisogno di un timestamp UNIX:

```Lua
local unix_timestamp = os.time()
print(unix_timestamp) -- Output: 1648830776
```

## Deep Dive:
La funzionalità di gestione della data e ora in Lua è diretta, offrendo essenzialmente quello che ti serve senza troppi fronzoli. Nato nei primi anni '90, Lua al tempo era uno dei pochi linguaggi a fornire una gestione delle date incorporata così semplice. Oggi ci sono alternative attraverso librerie esterne come `LuaDate` che offrono maggiore flessibilità se necessario. Ma per molti compiti, `os.date` e `os.time` fanno il lavoro. Ai fini dell'internazionalizzazione, ricorda che `os.date` si basa sulle impostazioni locali della tua macchina per il formato della data.

## See Also:
- [La documentazione ufficiale di Lua su os.date](https://www.lua.org/manual/5.4/manual.html#pdf-os.date)
