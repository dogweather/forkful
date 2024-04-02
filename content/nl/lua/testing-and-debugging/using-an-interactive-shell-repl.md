---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:10.069478-07:00
description: "REPL staat voor Read-Eval-Print Loop, een interactieve omgeving waar\
  \ je snel code kunt testen. Programmeurs gebruiken het om te experimenteren, te\u2026"
lastmod: '2024-03-13T22:44:50.939051-06:00'
model: gpt-4-0125-preview
summary: "REPL staat voor Read-Eval-Print Loop, een interactieve omgeving waar je\
  \ snel code kunt testen. Programmeurs gebruiken het om te experimenteren, te\u2026"
title: Het gebruik van een interactieve shell (REPL)
weight: 34
---

## Wat & Waarom?
REPL staat voor Read-Eval-Print Loop, een interactieve omgeving waar je snel code kunt testen. Programmeurs gebruiken het om te experimenteren, te debuggen en de eigenaardigheden van een taal te leren.

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
