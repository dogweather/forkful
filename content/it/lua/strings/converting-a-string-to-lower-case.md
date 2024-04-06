---
date: 2024-01-20 17:38:39.367255-07:00
description: "Come fare: La funzione `lower()` in Lua fa parte del repertorio standard\
  \ dal suo debutto; \xE8 semplice ed efficiente per il trattamento delle stringhe.\u2026"
lastmod: '2024-04-05T21:53:44.309112-06:00'
model: gpt-4-1106-preview
summary: "La funzione `lower()` in Lua fa parte del repertorio standard dal suo debutto;\
  \ \xE8 semplice ed efficiente per il trattamento delle stringhe."
title: Conversione di una stringa in minuscolo
weight: 4
---

## Come fare:
```Lua
-- Stringa originale
local stringaOriginale = "CIAO Mondo!"

-- Conversione in minuscolo
local stringaMinuscola = stringaOriginale:lower()

-- Stampa del risultato
print(stringaMinuscola)  -- Output: ciao mondo!
```

## Approfondimento
La funzione `lower()` in Lua fa parte del repertorio standard dal suo debutto; è semplice ed efficiente per il trattamento delle stringhe. Un'alternativa, prima dell'avvento di funzioni native come `lower()`, includeva la creazione di una funzione manuale mappando ogni lettera maiuscola alla sua corrispondente minuscola. Dettagli di implementazione riguardano solitamente le tavole ASCII o Unicode per la conversione dei caratteri.

## Vedi Anche
- [String Manipulation - Programming in Lua](https://www.lua.org/pil/20.1.html)
- [Lua 5.4 Reference Manual - Strings](https://www.lua.org/manual/5.4/manual.html#6.4)
