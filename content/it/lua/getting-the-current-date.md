---
title:                "Ottenere la data corrente"
date:                  2024-01-20T15:15:26.353231-07:00
html_title:           "Arduino: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Ottenere la data corrente in un programma Lua significa semplicemente acquisire le informazioni sulla data e ora in cui il programma è in esecuzione. I programmatori lo fanno per log, timestamp, funzionalità timer o per qualsiasi funzione che richieda la traccia temporale.

## How to:
In Lua, l'ottenimento della data corrente è piuttosto lineare. Ecco come:

```Lua
os.execute("date") -- Esegue il comando data del sistema
print(os.date()) -- Stampa la data e ora corrente secondo Lua

-- Manipolare il formato della data
print(os.date("%A, %B %d, %Y")) -- Stampa, per esempio, "Friday, October 08, 2021"
```

Sample output:

```
Fri Oct 8 14:22:56 2021
Fri Oct 8 14:22:56 2021
Friday, October 08, 2021
```

## Deep Dive
Lua utilizza la libreria standard C per fornire funzioni di data e ora attraverso il modulo `os`. Infatti, `os.date()` è basato sulla funzione `strftime` del C, che spiega la sintassi dei formati che possiamo usare.

Alternative? Alcune librerie Lua di terze parti estendono la gestione del tempo. Tuttavia, per l'uso basilare, il modulo `os` è più che sufficiente.

Dettagli di implementazione: Nella stampa della data, `os.date("*t")` fornisce una tabella con tutti i componenti della data (anno, mese, giorno, ecc.), mentre `os.date()` ritorna la data formattata come stringa.

## See Also
- [Lua 5.4 reference manual - os library](https://www.lua.org/manual/5.4/manual.html#6.9): Per ulteriori dettagli sul modulo `os` e le funzioni relative al tempo.
- [strftime - C documentation](https://www.cplusplus.com/reference/ctime/strftime/): La documentazione sulla funzione `strftime` di C per comprendere meglio i formati di data e ora.
