---
date: 2024-01-26 04:16:25.460610-07:00
description: "Hur g\xF6r man: F\xF6r att hoppa in i Lua's REPL, skriv bara `lua` i\
  \ din terminal. H\xE4r \xE4r ett exempel p\xE5 en session."
lastmod: '2024-03-13T22:44:38.040109-06:00'
model: gpt-4-0125-preview
summary: "F\xF6r att hoppa in i Lua's REPL, skriv bara `lua` i din terminal."
title: "Anv\xE4nda en interaktiv skal (REPL)"
weight: 34
---

## Hur gör man:
För att hoppa in i Lua's REPL, skriv bara `lua` i din terminal. Här är ett exempel på en session:

```Lua
> x = 10
> print(x * 2)
20
> t = {'apple', 'banana', 'cherry'}
> table.insert(t, 'date')
> for i, frukt in ipairs(t) do print(i, frukt) end
1	apple
2	banana
3	cherry
4	date
>
```
I sessionen deklarerar vi en variabel, utför grundläggande aritmetik, manipulerar en tabell och loopar igenom dess objekt.

## Djupdykning
Luas lätta natur gör dess REPL idealisk för prototyping. Den har funnits sedan Luas början i början av 1990-talet, inspirerad av tidigare interaktiva skal för språk som Lisp. Alternativ i andra språk inkluderar `irb` för Ruby och `python` för Python, var och en med sin egen uppsättning funktioner. Luas REPL är minimalistisk; därför kan den sakna avancerade funktioner som finns i andra, som komplexa felsökningsverktyg. För en mer robust upplevelse erbjuder verktyg som ZeroBrane Studio eller LuaDist's LuaRocks mer än den grundläggande REPL.

## Se även
- [Lua 5.4 Referensmanual - Den fristående Lua-tolken](https://www.lua.org/manual/5.4/manual.html#6)
- [ZeroBrane Studio](https://studio.zerobrane.com/)
- [LuaRocks](https://luarocks.org/)
