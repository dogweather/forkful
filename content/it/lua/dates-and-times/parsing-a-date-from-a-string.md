---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:48.322859-07:00
description: "L'analisi di una data da una stringa comporta la conversione delle rappresentazioni\
  \ testuali di date e orari in un formato che pu\xF2 essere facilmente\u2026"
lastmod: '2024-02-25T18:49:41.429398-07:00'
model: gpt-4-0125-preview
summary: "L'analisi di una data da una stringa comporta la conversione delle rappresentazioni\
  \ testuali di date e orari in un formato che pu\xF2 essere facilmente\u2026"
title: Analisi di una data da una stringa
---

{{< edit_this_page >}}

## Cosa e perché?
L'analisi di una data da una stringa comporta la conversione delle rappresentazioni testuali di date e orari in un formato che può essere facilmente manipolato, memorizzato o confrontato all'interno di un programma Lua. I programmatori svolgono questo compito per facilitare operazioni come la pianificazione, la registrazione o qualsiasi calcolo temporale e per colmare il divario tra i formati di data leggibili dall'uomo e i tipi di dati strutturati che un computer può elaborare efficientemente.

## Come fare:
Lua non ha supporto integrato per la manipolazione di date e orari oltre alla funzionalità limitata fornita dalle funzioni `os.date` e `os.time`. Tuttavia, queste possono essere sfruttate per un parsing di base, e per requisiti più complessi, può essere utilizzata la libreria esterna `luadate`.

**Utilizzando `os.date` e `os.time`:**
```lua
-- Convertire una data in formato leggibile dall'uomo in un timestamp e viceversa
local dateString = "2023-09-21 15:00:00"
local pattern = "(%d+)-(%d+)-(%d+) (%d+):(%d+):(%d+)"
local year, month, day, hour, minute, second = dateString:match(pattern)

local timestamp = os.time({
  year = year,
  month = month,
  day = day,
  hour = hour,
  min = minute,
  sec = second
})

-- Convertire il timestamp di nuovo in un formato leggibile dall'uomo
local formattedDate = os.date("%Y-%m-%d %H:%M:%S", timestamp)
print(formattedDate)  -- Output: 2023-09-21 15:00:00
```

**Utilizzando `luadate` (libreria di terze parti):**
Per utilizzare `luadate`, assicurati che sia installato tramite LuaRocks o il gestore di pacchetti di tua scelta. `luadate` aggiunge ampie capacità di parsing e manipolazione di date e orari.

```lua
local date = require('date')

-- Analizzare direttamente una stringa di data
local parsedDate = date.parse("2023-09-21 15:00:00")
print(parsedDate:fmt("%Y-%m-%d %H:%M:%S"))  -- Output: 2023-09-21 15:00:00

-- Aggiungere durate
local oneWeekLater = parsedDate:adddays(7)
print(oneWeekLater:fmt("%Y-%m-%d %H:%M:%S"))  -- Output: 2023-09-28 15:00:00
```

La libreria `luadate` offre un modo più intuitivo e potente per lavorare con le date, inclusi il parsing da stringhe, la formattazione e le operazioni aritmetiche sulle date, che semplifica notevolmente il lavoro con i dati temporali in Lua.
