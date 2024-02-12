---
title:                "Eliminazione di caratteri che corrispondono a un pattern"
aliases:
- /it/lua/deleting-characters-matching-a-pattern/
date:                  2024-01-20T17:43:16.686809-07:00
model:                 gpt-4-1106-preview
simple_title:         "Eliminazione di caratteri che corrispondono a un pattern"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? 
Cancellare caratteri corrispondenti a un pattern significa rimuovere sequenze specifiche di testo da una stringa. Lo facciamo per pulire i dati, validare input o semplificarli per elaborazioni successive.

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
