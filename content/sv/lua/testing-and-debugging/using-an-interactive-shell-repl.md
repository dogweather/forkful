---
title:                "Använda en interaktiv skal (REPL)"
aliases:
- /sv/lua/using-an-interactive-shell-repl/
date:                  2024-01-26T04:16:25.460610-07:00
model:                 gpt-4-0125-preview
simple_title:         "Använda en interaktiv skal (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Vad & Varför?
REPL står för Read-Eval-Print Loop, en interaktiv miljö där du snabbt kan testa kod. Programmerare använder den för att experimentera, felsöka och lära sig ett språks egenheter.

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
