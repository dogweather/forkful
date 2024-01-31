---
title:                "Использование интерактивной оболочки (REPL)"
date:                  2024-01-29T00:03:50.670617-07:00
model:                 gpt-4-0125-preview
simple_title:         "Использование интерактивной оболочки (REPL)"

category:             "Lua"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/lua/using-an-interactive-shell-repl.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Зачем?
REPL означает Цикл «Чтение-Выполнение-Вывод» (Read-Eval-Print Loop), интерактивная среда, где вы можете быстро тестировать код. Программисты используют его для экспериментов, отладки и изучения особенностей языка.

## Как использовать:
Чтобы войти в REPL Lua, просто введите `lua` в вашем терминале. Вот пример сессии:

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
В сессии мы объявляем переменную, выполняем базовые арифметические операции, манипулируем таблицей и перебираем её элементы.

## Подробнее
Легковесность Lua делает её REPL идеальной для прототипирования. REPL существует с момента создания Lua в начале 1990-х годов, вдохновленный предыдущими интерактивными оболочками для языков, таких как Lisp. Альтернативы в других языках включают `irb` для Ruby и `python` для Python, каждая со своим набором функций. REPL Lua минималистичен; таким образом, ему может не хватать продвинутых функций, найденных в других, например, сложных инструментов отладки. Для более насыщенного опыта инструменты, такие как ZeroBrane Studio или LuaDist's LuaRocks, предлагают больше, чем основной REPL.

## Смотрите также
- [Руководство по Lua 5.4 - Автономный интерпретатор Lua](https://www.lua.org/manual/5.4/manual.html#6)
- [ZeroBrane Studio](https://studio.zerobrane.com/)
- [LuaRocks](https://luarocks.org/)
