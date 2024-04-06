---
date: 2024-01-20 17:58:16.785664-07:00
description: 'How to: In italiano: "## Come Fare:" Esempio di codice in Lua per cercare
  e sostituire testo.'
lastmod: '2024-03-13T22:44:43.544210-06:00'
model: gpt-4-1106-preview
summary: In italiano.
title: Ricerca e sostituzione del testo
weight: 10
---

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
