---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:10.069478-07:00
description: 'Hoe: Om in Lua''s REPL te springen, voer je gewoon `lua` in je terminal
  in. Hier volgt een voorbeeldsessie.'
lastmod: '2024-03-13T22:44:50.939051-06:00'
model: gpt-4-0125-preview
summary: Om in Lua's REPL te springen, voer je gewoon `lua` in je terminal in.
title: Het gebruik van een interactieve shell (REPL)
weight: 34
---

## Hoe:
Om in Lua's REPL te springen, voer je gewoon `lua` in je terminal in. Hier volgt een voorbeeldsessie:

```Lua
> x = 10
> print(x * 2)
20
> t = {'apple', 'banana', 'cherry'}
> table.insert(t, 'date')
> for i, fruit in ipairs(t) do print(i, fruit) end
1	apple
2	banana
3	cherry
4	date
>
```
In de sessie verklaren we een variabele, voeren we basis rekenkunde uit, manipuleren we een tabel en lopen we door de items heen.

## Diepgaand
De lichtgewicht aard van Lua maakt zijn REPL ideaal voor prototyping. Het bestaat al sinds de vroege jaren 90, toen Lua ontstond, geïnspireerd door eerdere interactieve shells voor talen zoals Lisp. Alternatieven in andere talen zijn onder andere `irb` voor Ruby en `python` voor Python, elk met hun eigen reeks functies. Lua's REPL is minimalistisch; daardoor kan het geavanceerde functies missen die anderen wel hebben, zoals complexe debugging tools. Voor een uitgebreidere ervaring bieden tools zoals ZeroBrane Studio of LuaDist's LuaRocks meer dan de basis REPL.

## Zie ook
- [Lua 5.4 Referentiehandleiding - De Standalone Lua Interpreter](https://www.lua.org/manual/5.4/manual.html#6)
- [ZeroBrane Studio](https://studio.zerobrane.com/)
- [LuaRocks](https://luarocks.org/)
