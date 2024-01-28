---
title:                "Trovare la lunghezza di una stringa"
date:                  2024-01-20T17:47:59.713760-07:00
model:                 gpt-4-1106-preview
simple_title:         "Trovare la lunghezza di una stringa"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Scoprire la lunghezza di una stringa significa sapere quanti caratteri contiene. Lo facciamo quando, ad esempio, dobbiamo validare l'input, manipolare testi o semplicemente per avere informazioni sulla struttura dei dati che stiamo trattando.

## How to:
In Lua, ottenere la lunghezza di una stringa è un gioco da ragazzi. Usa l'operatore `#` per farlo. Ecco un esempio:

```Lua
local saluto = "Ciao, mondo!"
local lunghezza = #saluto
print(lunghezza)  -- Stampa: 12
```

Stai mostrando la lunghezza di una stringa italiana, quindi aspettati un carattere in più per le accentate!

```Lua
local parola_con_accento = "perché"
print(#parola_con_accento)  -- Stampa: 6
```

## Deep Dive
In Lua, la lunghezza di una stringa corrisponde al numero di byte che la compongono. Questo diventa particolarmente rilevante con le stringhe UTF-8, dove alcuni caratteri possono occupare più di un byte. Prima di Lua 5.3, non c'erano funzioni native per gestire correttamente la lunghezza delle stringhe UTF-8, spesso si ricorreva a librerie esterne. Da Lua 5.3, puoi usare `utf8.len()` per ottenere la lunghezza corretta di stringhe UTF-8. Ecco come:

```Lua
local utf8 = require("utf8")
local parola = "caffè"
print(utf8.len(parola))  -- Stampa: 5, non 6!
```

Ricorda: `#` conta i byte, `utf8.len()` conta i caratteri.

## See Also
- [Programming in Lua (Official Book)](https://www.lua.org/pil/contents.html)
- [Lua 5.4 Reference Manual](https://www.lua.org/manual/5.4/)

Puoi approfondire attraverso la documentazione ufficiale e le risorse comunitarie per dominare la gestione delle stringhe in Lua. Buona programmazione!
