---
date: 2024-01-26 04:16:12.385001-07:00
description: "REPL oznacza Read-Eval-Print Loop (P\u0119tla Czytaj-Wykonaj-Wydrukuj),\
  \ interaktywne \u015Brodowisko, gdzie mo\u017Cna szybko testowa\u0107 kod. Programi\u015B\
  ci u\u017Cywaj\u0105 go do\u2026"
lastmod: '2024-03-11T00:14:08.729400-06:00'
model: gpt-4-0125-preview
summary: "REPL oznacza Read-Eval-Print Loop (P\u0119tla Czytaj-Wykonaj-Wydrukuj),\
  \ interaktywne \u015Brodowisko, gdzie mo\u017Cna szybko testowa\u0107 kod. Programi\u015B\
  ci u\u017Cywaj\u0105 go do\u2026"
title: Korzystanie z interaktywnego shella (REPL)
---

{{< edit_this_page >}}

## Co i dlaczego?
REPL oznacza Read-Eval-Print Loop (Pętla Czytaj-Wykonaj-Wydrukuj), interaktywne środowisko, gdzie można szybko testować kod. Programiści używają go do eksperymentowania, debugowania i poznawania osobliwości języka.

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
