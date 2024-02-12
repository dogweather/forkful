---
title:                "Робота з JSON"
date:                  2024-02-03T19:23:44.187452-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/lua/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?
Робота з JSON у Lua полягає в аналізі рядків, відформатованих у JSON, до таблиць Lua і навпаки, що дозволяє легко обмінюватися даними між програмами Lua та веб-службами або зовнішніми API. Програмісти роблять це, щоб використовувати легкий і легко аналізований формат JSON для ефективного зберігання даних, конфігурації або комунікації з API.

## Як це зробити:
Lua не включає вбудованої бібліотеки для обробки JSON. Таким чином, однією з популярних сторонніх бібліотек є `dkjson`, яку ви можете легко використовувати для кодування і декодування JSON. Спочатку переконайтеся, що встановили `dkjson`, наприклад, через LuaRocks (`luarocks install dkjson`), а потім слідуйте прикладам нижче.

### Декодування JSON до таблиці Lua
```lua
local dkjson = require "dkjson"

local jsonString = '{"name": "Lua Programmer", "age": 30, "languages": ["Lua", "JavaScript"]}'
local luaTable, pos, err = dkjson.decode(jsonString, 1, nil)
if err then
  print ("Помилка:", err)
else
  print("Ім'я:", luaTable.name) -- Вивід: Ім'я: Lua Programmer
  print("Вік:", luaTable.age) -- Вивід: Вік: 30
  print("Мови:", table.concat(luaTable.languages, ", ")) -- Вивід: Мови: Lua, JavaScript
end
```

### Кодування таблиці Lua в JSON
```lua
local dkjson = require "dkjson"

local luaTable = {
  name = "Lua Programmer",
  age = 30,
  languages = { "Lua", "JavaScript" }
}

local jsonString = dkjson.encode(luaTable, { indent = true })
print(jsonString)
```

Приклад виводу для кодування:
```json
{
  "age": 30,
  "languages": [
    "Lua",
    "JavaScript"
  ],
  "name": "Lua Programmer"
}
```

Ці прості приклади демонструють, як працювати з JSON у Lua, роблячи легким інтеграцію програм Lua з різними веб-технологіями та зовнішніми API. Пам'ятайте, хоча в цих прикладах використовується `dkjson`, інші бібліотеки, такі як `cjson` та `RapidJSON`, також можуть бути підходящими альтернативами залежно від потреб вашого проекту.
