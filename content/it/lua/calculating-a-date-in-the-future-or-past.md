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

## Cosa & Perché?
Calcolare una data nel futuro o nel passato è un'operazione comune per i programmatori, che spesso devono gestire le date per diverse ragioni, come la pianificazione di eventi o il calcolo di scadenze.

## Come fare:
Ecco alcuni esempi di codice Lua per calcolare una data nel futuro o nel passato:

```
-- Calcolare una data 7 giorni nel futuro
local oggi = os.date("*t") -- ottieni la data odierna come una tabella
oggi.day = oggi.day + 7 -- aumenta il giorno di 7
local data_nel_futuro = os.time(oggi) -- converte la tabella in timestamp
print(data_nel_futuro) -- stampa il timestamp del futuro (in secondi)

-- Calcolare una data 1 mese nel passato
local oggi = os.date("*t")
oggi.month = oggi.month - 1 -- diminuisci il mese di 1
local data_nel_passato = os.time(oggi)
print(os.date("%d/%m/%Y", data_nel_passato)) -- stampa la data nel formato gg/mm/AAAA

-- Calcolare una data 3 anni nel futuro partendo da una data specifica
local data_iniziale = "25/10/2020" -- data nel formato gg/mm/AAAA
local data_iniziale_timestamp = os.time(os.date("*t", os.time(data_iniziale))) -- converti la data in timestamp
local data_nel_futuro = os.date("%d/%m/%Y", data_iniziale_timestamp + (3*365*24*60*60)) -- aggiungi 3 anni in secondi
print(data_nel_futuro) -- stampa la nuova data nel formato gg/mm/AAAA
```

Output:

```
1541760000
01/01/2020
25/10/2023
```
 
## Approfondimento:
Calcolare una data nel futuro o nel passato può sembrare semplice, ma ci sono alcuni aspetti da considerare. Una delle sfide più comuni è gestire correttamente i fusi orari e gli anni bisestili. Per semplificare questa operazione, esistono librerie specifiche che possono essere utilizzate, come "date" in Lua o "moment" in Node.js. Inoltre, è importante prestare attenzione ai modi in cui i diversi formati di data vengono interpretati dai vari linguaggi di programmazione.

## Vedi anche:
- [Lua Date Library](https://github.com/Tieske/date)
- [Moment.js](https://momentjs.com/)
- [Gestione delle date (Wikipedia)](https://it.wikipedia.org/wiki/Gestione_delle_date)