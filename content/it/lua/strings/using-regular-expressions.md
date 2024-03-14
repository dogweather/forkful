---
date: 2024-01-19
description: "Le espressioni regolari sono modelli per cercare corrispondenze in stringhe\
  \ di testo. I programmatori le usano per validare, estrarre e manipolare dati\u2026"
lastmod: '2024-03-13T22:44:43.548423-06:00'
model: unknown
summary: "Le espressioni regolari sono modelli per cercare corrispondenze in stringhe\
  \ di testo. I programmatori le usano per validare, estrarre e manipolare dati\u2026"
title: Utilizzo delle espressioni regolari
---

{{< edit_this_page >}}

## What & Why?
Le espressioni regolari sono modelli per cercare corrispondenze in stringhe di testo. I programmatori le usano per validare, estrarre e manipolare dati con precisione e velocità.

## How to:
```Lua
local testo = "Oggi è il 3 aprile 2023"
local pattern = "%d+ %a+ %d+" -- Cerca data nel formato "giorno mese anno"

-- Trova la corrispondenza
local match = string.match(testo, pattern)
print(match)  -- Output: 3 aprile 2023

-- Sostituisci la corrispondenza
local testo_modificato = string.gsub(testo, pattern, "1 maggio 2024")
print(testo_modificato)  -- Output: Oggi è il 1 maggio 2024
```

## Deep Dive
Le espressioni regolari, o regex, nascono negli anni '50 e si sono evolute in diversi standard. In Lua, le espressioni regolari sono più semplici rispetto a quelle in altri linguaggi e sono chiamate pattern matching. Alternativamente, si possono utilizzare librerie esterne come `lrexlib` o `LPeg` per funzionalità più avanzate. Lua implementa i pattern matching limitando la complessità per mantenere leggerezza e velocità.

## See Also
- [Lua 5.4 Reference Manual - Patterns](https://www.lua.org/manual/5.4/manual.html#6.4.1)
- [LPeg Library](http://www.inf.puc-rio.br/~roberto/lpeg/)
