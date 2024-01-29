---
title:                "Het gebruik van een interactieve shell (REPL)"
date:                  2024-01-28T22:09:10.069478-07:00
model:                 gpt-4-0125-preview
simple_title:         "Het gebruik van een interactieve shell (REPL)"
programming_language: "Lua"
category:             "Lua"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/lua/using-an-interactive-shell-repl.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
