---
title:                "Работа с JSON"
aliases:
- ru/lua/working-with-json.md
date:                  2024-01-29T00:04:07.499096-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/lua/working-with-json.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
