---
date: 2024-01-20 17:43:16.686809-07:00
description: 'How to: In Lua, puoi usare la funzione `string.gsub` per eliminare caratteri
  o pattern. Ecco un esempio pratico.'
lastmod: '2024-03-13T22:44:43.543480-06:00'
model: gpt-4-1106-preview
summary: In Lua, puoi usare la funzione `string.gsub` per eliminare caratteri o pattern.
title: Eliminazione di caratteri che corrispondono a un pattern
weight: 5
---

## How to:
In Lua, puoi usare la funzione `string.gsub` per eliminare caratteri o pattern. Ecco un esempio pratico:

```Lua
local testo_originale = "Lua è fantastico, 123!"
local pattern = "%D" -- pattern che individua tutti i caratteri non numerici
local testo_pulito, num_sostituzioni = testo_originale:gsub(pattern, "")

print(testo_pulito)  -- Output: 123
print("Sostituzioni effettuate: ", num_sostituzioni)  -- Output: Sostituzioni effettuate:  18
```

Qui `%D` trova ogni carattere che non è un numero (`%d` troverebbe i numeri). La funzione `gsub` li sostituisce con una stringa vuota, cioè li cancella.

## Deep Dive
La funzione `gsub` in Lua è erede diretta delle espressioni regolari usate in ambienti di programmazione UNIX fin dagli anni '70. Lua usa un sistema più semplice rispetto ad altri linguaggi, mirando alla performance e alla leggibilità.

Hai alternative per eliminare pattern: puoi iterare sui caratteri o usare funzioni di libreria esterne per pattern matching più avanzato, ma `gsub` è il modo più comune e diretto fornito nativamente da Lua.

Dal punto di vista dell'implementazione, `gsub` crea una nuova stringa anziché modificare quella originale, perché in Lua le stringhe sono immutabili. Questo comporta un maggior controllo sui dati ma anche potenziali overhead in termini di memoria.

## See Also
- [Programming in Lua (book)](https://www.lua.org/pil/contents.html) per un'introduzione completa.
- [Lua 5.4 Reference Manual](https://www.lua.org/manual/5.4/) per i dettagli sulla funzione `gsub` e pattern matching.
