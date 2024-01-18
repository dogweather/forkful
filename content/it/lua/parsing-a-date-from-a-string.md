---
title:                "Analisi di una data da una stringa"
html_title:           "Lua: Analisi di una data da una stringa"
simple_title:         "Analisi di una data da una stringa"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Cosa e Perché?
Parsing una data da una stringa è il processo di estrarre una data da una frase o stringa di testo. I programmatori spesso lo fanno quando devono manipolare dati di tipo data all'interno dei loro codici.

## Come fare:
Ecco un esempio semplice di come eseguire il parsing di una data da una stringa in Lua utilizzando la libreria `os.date`:

```Lua
local dateString = "20/02/2020"
local day, month, year = dateString:match("(%d+)%/(%d+)%/(%d+)")
local dateTable = {year = year, month = month, day = day}
local parsedDate = os.date("%d.%m.%Y", os.time(dateTable))
-- output: "20.02.2020"
```

## Approfondimento:
Il parsing delle date da stringhe è diventato una pratica comune nei linguaggi di programmazione moderni grazie all'ampia diffusione dei formati di data standardizzati, come ad esempio il formato ISO 8601. In Lua, esistono anche altre librerie utilizzabili per il parsing delle date, come ad esempio `dateparser` e `date`. La scelta della libreria da utilizzare dipende dalle esigenze specifiche del progetto.

## Vedi anche:
- [Documentazione ufficiale di Lua sulle funzioni di data e orario](https://www.lua.org/manual/5.4/manual.html#6.9)
- [Libreria `dateparser` per il parsing delle date in Lua](https://github.com/Tieske/dateparser)
- [Libreria `date` per il parsing delle date in Lua](https://github.com/daurnimator/date)