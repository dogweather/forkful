---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:07.499096-07:00
description: "JSON (JavaScript Object Notation) \u0438\u0441\u043F\u043E\u043B\u044C\
  \u0437\u0443\u0435\u0442\u0441\u044F \u0434\u043B\u044F \u0445\u0440\u0430\u043D\
  \u0435\u043D\u0438\u044F \u0438 \u043F\u0435\u0440\u0435\u0434\u0430\u0447\u0438\
  \ \u0434\u0430\u043D\u043D\u044B\u0445. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\
  \u043C\u0438\u0441\u0442\u044B \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u044E\
  \u0442 JSON, \u043F\u043E\u0442\u043E\u043C\u0443 \u0447\u0442\u043E \u043E\u043D\
  \ \u043B\u0435\u0433\u043A\u0438\u0439, \u043B\u0435\u0433\u043A\u043E \u0447\u0438\
  \u0442\u0430\u0435\u043C \u0438 \u043F\u0438\u0448\u0435\u0442\u0441\u044F\u2026"
lastmod: '2024-03-13T22:44:45.327696-06:00'
model: gpt-4-0125-preview
summary: "JSON (JavaScript Object Notation) \u0438\u0441\u043F\u043E\u043B\u044C\u0437\
  \u0443\u0435\u0442\u0441\u044F \u0434\u043B\u044F \u0445\u0440\u0430\u043D\u0435\
  \u043D\u0438\u044F \u0438 \u043F\u0435\u0440\u0435\u0434\u0430\u0447\u0438 \u0434\
  \u0430\u043D\u043D\u044B\u0445."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 JSON"
weight: 38
---

## Что и Зачем?

JSON (JavaScript Object Notation) используется для хранения и передачи данных. Программисты используют JSON, потому что он легкий, легко читаем и пишется человеком, а также легко анализируется и генерируется машиной.

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
