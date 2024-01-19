---
title:                "Calcolare una data nel futuro o nel passato"
html_title:           "Lua: Calcolare una data nel futuro o nel passato"
simple_title:         "Calcolare una data nel futuro o nel passato"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Cos'è e perché? (What & Why?)

Calcolare una data nel futuro o nel passato significa manipolare e calcolare le date rispetto al tempo corrente. I programmatori lo fanno per gestire eventi pianificati, reminders, programmazione di task e molto altro.

## Come fare: (How to)

Lua offre la funzione `os.date`, che ritorna una stringa di formato di data/ora. Per calcolare una data futura o passata, dobbiamo aggiungere o sottrarre secondi al tempo corrente.

```Lua
--Calcola una Data Futura
local secondsPerDay = 60 * 60 * 24
local now = os.time()
local future = now + (10 * secondsPerDay) -- 10 days into the future
print(os.date("%c", future))

--Calcola una Data Passata
local past = now - (7 * secondsPerDay) -- 7 days into the past
print(os.date("%c", past))
```

L'outuput sarà formato data/ora nel futuro o nel passato.

## Analisi Dettagliata (Deep Dive)

Historicamente, Lua ha fornito potenti funzioni per manipolare le date e l'ora, facilitandone le operazioni. Naturalmente, le soluzioni possono variare. Invece di lavorare con i secondi, è possibile utilizzare librerie esterne come date.lua o Chronos per ottenere più funzionalità e maggiore precisione.

In termini di implementazione, `os.time()` ritorna il numero di secondi trascorsi dal 1 Gennaio 1970 (conosciuto come l'epoca Unix). Quando aggiungiamo o sottraiamo secondi, stiamo effettivamente calcolando il tempo relativo a quella data base.

## Vedi Anche (See Also)

- Documentazione ufficiale di Lua `os.date` e `os.time`: https://www.lua.org/manual/5.4/manual.html#6.9
- Libreria date.lua: https://github.com/Tieske/date
- Libreria Chronos: https://github.com/Adriweb/chronos.lua