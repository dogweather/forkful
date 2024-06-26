---
date: 2024-01-26 04:16:12.385001-07:00
description: "Jak to zrobi\u0107: Aby wej\u015B\u0107 do REPL Lua, wystarczy wpisa\u0107\
  \ `lua` w terminalu. Oto przyk\u0142adowa sesja."
lastmod: '2024-03-13T22:44:35.541579-06:00'
model: gpt-4-0125-preview
summary: "Aby wej\u015B\u0107 do REPL Lua, wystarczy wpisa\u0107 `lua` w terminalu."
title: Korzystanie z interaktywnego shella (REPL)
weight: 34
---

## Jak to zrobić:
Aby wejść do REPL Lua, wystarczy wpisać `lua` w terminalu. Oto przykładowa sesja:

```Lua
> x = 10
> print(x * 2)
20
> t = {'apple', 'banana', 'cherry'}
> table.insert(t, 'date')
> for i, owoc in ipairs(t) do print(i, owoc) end
1	apple
2	banana
3	cherry
4	date
>
```
W sesji deklarujemy zmienną, wykonujemy podstawowe działania arytmetyczne, manipulujemy tabelą i iterujemy przez jej elementy.

## Dogłębna analiza
Lekka natura Lua sprawia, że jej REPL jest idealny do tworzenia prototypów. Istnieje on od początków Lua na początku lat 90., inspirowany wcześniejszymi interaktywnymi powłokami dla języków takich jak Lisp. Alternatywy w innych językach to `irb` dla Ruby i `python` dla Pythona, każda z własnym zestawem funkcji. REPL Lua jest minimalistyczny; tym samym może brakować mu zaawansowanych funkcji, znajdowanych w innych, takich jak skomplikowane narzędzia do debugowania. Dla bardziej rozbudowanego doświadczenia narzędzia takie jak ZeroBrane Studio czy LuaDist's LuaRocks oferują więcej niż podstawowy REPL.

## Zobacz również
- [Lua 5.4 Reference Manual - Samodzielny Interpreter Lua](https://www.lua.org/manual/5.4/manual.html#6)
- [ZeroBrane Studio](https://studio.zerobrane.com/)
- [LuaRocks](https://luarocks.org/)
