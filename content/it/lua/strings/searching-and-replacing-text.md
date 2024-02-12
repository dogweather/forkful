---
title:                "Ricerca e sostituzione del testo"
aliases: - /it/lua/searching-and-replacing-text.md
date:                  2024-01-20T17:58:16.785664-07:00
model:                 gpt-4-1106-preview
simple_title:         "Ricerca e sostituzione del testo"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?
In italiano: "## Cosa e Perché?"

Cercare e sostituire testo (search and replace) è come trovare una parola in un libro e cambiarla con un'altra. I programmatori lo fanno per correggere errori, aggiornare informazioni, o manipolare dati rapidamente.

## How to:
In italiano: "## Come Fare:"

Esempio di codice in Lua per cercare e sostituire testo:

```Lua
local testo = "Ciao mondo, programmazione in Lua!"
local ricerca = "mondo"
local sostituzione = "universo"
local risultato = testo:gsub(ricerca, sostituzione)
print(risultato)
```

Output:
```
Ciao universo, programmazione in Lua!
```

## Deep Dive
In italiano: "## Approfondimento"

La funzione `gsub` in Lua è l'eroe non celebrato della manipolazione delle stringhe. Nata dalle esigenze di efﬁcienza e flessibilità negli script, `gsub` può non solo sostituire semplici parole ma anche eseguire sostituzioni complesse usando pattern matching. Altre opzioni, come le espressioni regolari, non sono supportate nativamente in Lua, ma possono essere implementate con librerie esterne. L'implementazione in Lua è lineare, semplice e performante nel suo contesto di utilizzo.

## See Also
In italiano: "## Vedi Anche"

- Documentazione ufficiale Lua su `string.gsub`: [www.lua.org/manual/5.4/manual.html#pdf-string.gsub](https://www.lua.org/manual/5.4/manual.html#pdf-string.gsub)
- Guida al pattern matching in Lua: [www.lua.org/pil/20.2.html](https://www.lua.org/pil/20.2.html)
- Repository Lua su GitHub: [github.com/lua/lua](https://github.com/lua/lua)
