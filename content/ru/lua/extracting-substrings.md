---
title:                "Извлечение подстрок"
date:                  2024-01-28T23:57:44.713438-07:00
model:                 gpt-4-0125-preview
simple_title:         "Извлечение подстрок"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/lua/extracting-substrings.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и зачем?
Извлечение подстрок означает выделение конкретного фрагмента строки. Программисты делают это для изоляции, анализа или манипуляции с конкретными данными внутри большего текста.

## Как это сделать:
В Lua используйте функцию `string.sub`:

```lua
local text = "Hello, Lua!"
-- Извлекаем 'Hello'
print(string.sub(text, 1, 5)) -- Вывод: Hello

-- Получаем 'Lua'
print(string.sub(text, 8, 11)) -- Вывод: Lua
```

Или получите последние символы с использованием отрицательных индексов:

```lua
-- Выделяем 'Lua!' с конца
print(string.sub(text, -4)) -- Вывод: Lua!
```

Используйте шаблоны для поиска и извлечения:

```lua
local phrase = "The quick brown fox jumps"
-- Соответствие и извлечение 'quick'
print(phrase:match("(%a+) quick")) -- Вывод: The
```

## Подробно
В раннем программировании обработка строк была ручной и громоздкой, часто требующей циклов и условных операторов. `string.sub` в Lua является частью его более богатой библиотеки строк, делая манипуляции со строками легкими. Альтернативы `string.sub` включают сопоставление с образцом с помощью `string.match`, которое более мощное, но может быть избыточным для простых задач.

`string.sub` и сопоставление с образцом основаны на функциях C из-за корней Lua в C. В Lua вы не найдете обширной стандартной библиотеки для строк по сравнению с языками, такими как Python; она придерживается основ, ценя простоту и эффективность. Помните, что индексы в Lua начинаются с 1, а не с 0.

## Смотрите также
- Руководство по Lua 5.4 о строках: [www.lua.org/manual/5.4/manual.html#6.4](https://www.lua.org/manual/5.4/manual.html#6.4)
- 'Программирование на Lua' (4-е издание), особенно глава о строках: [www.lua.org/pil/contents.html](https://www.lua.org/pil/contents.html)
