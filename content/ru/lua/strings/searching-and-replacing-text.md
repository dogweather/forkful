---
title:                "Поиск и замена текста"
date:                  2024-01-29T00:03:26.807896-07:00
model:                 gpt-4-0125-preview
simple_title:         "Поиск и замена текста"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/lua/searching-and-replacing-text.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и зачем?
Поиск и замена текста означает замену определенных строк в текстовом блоке на другие. Программисты делают это для задач, таких как исправление ошибок, обновление информации или форматирование данных.

## Как это сделать:
Функция `string.gsub` в Lua - это ваш инструмент для поиска и замены. Работает она так:

```lua
local text = "Быстрая коричневая лиса перепрыгнула через ленивую собаку."
local searchText = "ленивую"
local replaceWith = "энергичную"

local result = string.gsub(text, searchText, replaceWith)

print(result)
```

Вывод:

```
Быстрая коричневая лиса перепрыгнула через энергичную собаку.
```

Чтобы заменить ВСЕ вхождения, `gsub` делает это по умолчанию:

```lua
local text = "Яблоки сладкие. Яблоки сочные."
local result = string.gsub(text, "Яблоки", "Апельсины")

print(result)
```

Вывод:

```
Апельсины сладкие. Апельсины сочные.
```

## Подробности
Поиск и замена текста не уникальны для Lua; это общая функция в языках программирования. `string.gsub` в Lua возвращает к своим корням манипуляции со строками, предлагая прямолинейный подход к обработке шаблонов и замен.

Исторически, `gsub` (глобальная замена) был повлиян командой `sed` в Unix и мощными возможностями сопоставления шаблонов в Perl. Шаблоны Lua, хотя и проще, чем регулярные выражения, найденные в других языках, все же могут обрабатывать сложные совпадения с небольшим творчеством.

Альтернативы `string.gsub` включают ручное итерирование по строкам и конструирование замен - более подверженный ошибкам метод. Для обработки больших объемов текста можно использовать специализированные библиотеки разбора.

С точки зрения реализации, `gsub` может принимать функцию в качестве аргумента замены, позволяя программно контролировать замену.

```lua
local result = string.gsub(text, "(%a+)", function(word)
  return #word > 4 and word:upper() or word
end)
```

Этот фрагмент будет переводить слова длиннее четырёх символов в верхний регистр.

## Смотрите также
- Книга [Программирование на Lua](https://www.lua.org/pil/), предоставляет глубокие знания концептов программирования Lua.
- Для полного ознакомления с возможностями строковых шаблонов Lua, проверьте [Руководство по Lua 5.4](https://www.lua.org/manual/5.4/manual.html#6.4.1).