---
title:                "Estrarre una data da una stringa"
date:                  2024-01-20T15:37:29.132048-07:00
html_title:           "Arduino: Estrarre una data da una stringa"
simple_title:         "Estrarre una data da una stringa"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Analizzare una data da una stringa significa estrarre informazioni come il giorno, il mese e l'anno da testo. I programmatori lo fanno per manipolare e confrontare date o per convertirle in formati diversi.

## How to:
Ecco come fare in Lua. Supponiamo di avere una stringa di data nel formato "GG/MM/AAAA".

```lua
local data_str = "23/04/2023"

-- Funzione per l'analisi della data
function parseDate(str)
  local giorno, mese, anno = str:match("(%d%d)/(%d%d)/(%d%d%d%d)")
  return { giorno = tonumber(giorno), mese = tonumber(mese), anno = tonumber(anno) }
end

-- Utilizzo della funzione
local data = parseDate(data_str)
print(data.giorno, data.mese, data.anno) -- Output: 23 4 2023
```
## Deep Dive
In Lua non esiste una libreria standard per l'analisi delle date, quindi spesso si usa `string.match` con le espressioni regolari. Si può implementare una funzione personalizzata come quella sopra.

Prima di Lua 5.1, si doveva fare affidamento sulle funzioni di data/ora del sistema operativo. Con l'introduzione dei pattern matching, diventò più facile estraí intellerentemente i dati dalle stringhe.

Come alternativa, si potrebbe usare os.date e os.time per lavorare con timestamp Unix. Questi forniscono metodi per manipolare date e orari, ma sarebbe necessario convertire prima il formato della data in un timestamp.

## See Also
- Documentazione ufficiale Lua `os.date` e `os.time`: https://www.lua.org/manual/5.4/manual.html#6.9
- Guida ai pattern matching in Lua: https://www.lua.org/pil/20.2.html
