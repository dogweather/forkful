---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:07.499096-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u0440\u0430\u0437\u0431\
  \u0435\u0440\u0435\u043C \u043D\u0435\u043A\u043E\u0442\u043E\u0440\u044B\u0435\
  \ JSON."
lastmod: '2024-03-13T22:44:45.327696-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u0440\u0430\u0437\u0431\u0435\
  \u0440\u0435\u043C \u043D\u0435\u043A\u043E\u0442\u043E\u0440\u044B\u0435 JSON."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 JSON"
weight: 38
---

## Как это сделать:
Давайте разберем некоторые JSON.

```lua
-- Убедитесь, что у вас есть модуль 'dkjson' или другая библиотека для работы с JSON.
local dkjson = require 'dkjson'

local jsonString = '{"name":"John", "age":30, "city":"New York"}'

-- Разбор JSON строки в таблицу Lua.
local person, pos, err = dkjson.decode(jsonString, 1, nil)
if err then
    print("Ошибка:", err)
else
    print(person.name)  -- Вывод: John
end

-- Конвертация таблицы Lua в строку JSON.
local personTable = { name = "Jane", age = 25, city = "Los Angeles" }
local jsonOutput = dkjson.encode(personTable)
print(jsonOutput)  -- Вывод: {"age":25,"city":"Los Angeles","name":"Jane"}
```

Теперь давайте обработаем массивы.

```lua
local jsonArrayString = '[{"name":"John"}, {"name":"Jane"}]'

-- Разбор JSON строки с массивом в таблицу Lua.
local peopleArray, _, err = dkjson.decode(jsonArrayString)
if err then
    print("Ошибка:", err)
else
    for i, person in ipairs(peopleArray) do
        print(person.name)  -- Вывод: John\nJane
    end
end
```

## Подробнее
JSON стал de facto стандартом для API, опережая XML благодаря своей невербозности. Есть альтернативы, такие как YAML, который еще более читаем, но не так широко используется в API. В Lua нет встроенной поддержки JSON, поэтому вам нужна библиотека вроде 'dkjson' или 'cjson'. Особенности реализации в Lua включают в себя обработку различий типов, таких как массивы и объекты, и конвертацию между `nil` в Lua и `null` в JSON.

## Смотрите также
- [Библиотека dkjson на GitHub](https://github.com/LuaDist/dkjson)
- [Официальный сайт JSON](https://www.json.org/json-en.html)
- [Программирование на Lua (первое издание)](https://www.lua.org/pil/contents.html) для изучения основ Lua.
