---
title:                "Поиск длины строки"
date:                  2024-01-28T23:57:47.186264-07:00
model:                 gpt-4-0125-preview
simple_title:         "Поиск длины строки"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/lua/finding-the-length-of-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Найти длину строки - значит выяснить, сколько символов она содержит. Программисты делают это для проверки ввода, манипуляции с текстом или просто для подсчёта символов для различных задач.

## Как это сделать:

В Lua длину строки можно получить с помощью оператора `#`. Просто и быстро.

```lua
local myString = "Hello, Lua!"
print(#myString)  -- Вывод: 11
```

Что если ваша строка содержит символы новой строки или является пустой?

```lua
local stringWithNewline = "Hello\nLua!"
local emptyString = ""
print(#stringWithNewline)  -- Вывод: 10
print(#emptyString)         -- Вывод: 0
```

Даже с символами новой строки Lua считает каждый символ. И да, длина пустой строки равна 0.

## Подробнее

В старые времена строки в некоторых языках были более сложными. Вам могли понадобиться функции или методы, чтобы получить длину строки. Сегодня, в Lua, это так же прямолинейно, как использование оператора `#`. 

Альтернативы? Если вы работаете с символами Unicode, оператор `#` может давать сбой с многобайтовыми символами. В этом случае стоит изучить библиотеки вроде `utf8`. Начиная с Lua 5.3, эта встроенная библиотека появилась в языке.

```lua
local unicodeString = "こんにちは" -- Это "Привет" по-японски
print(#unicodeString)  -- Вывод может удивить, если вы не готовы к многобайтовым символам!
print(utf8.len(unicodeString))  -- Вывод: 5 символов, как и ожидалось
```

Деталь, на которую стоит обратить внимание: Lua хранит строки неизменяемыми и внутренне использует механизм, называемый интернированием строк. Это удобно, поскольку это позволяет экономить память и делает операции с длиной строки быстрыми.

## Смотрите также

- Руководство по Lua 5.4: Манипуляции со строками – https://www.lua.org/manual/5.4/manual.html#6.4
- Функция `utf8.len` – Погружение в корректную работу с Unicode строками – https://www.lua.org/manual/5.4/manual.html#pdf-utf8.len
- Немного истории Lua и информация об интернировании строк – https://www.lua.org/doc/hopl.pdf