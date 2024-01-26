---
title:                "Робота з JSON"
html_title:           "Arduino: Робота з JSON"
simple_title:         "Робота з JSON"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/lua/working-with-json.md"
---

{{< edit_this_page >}}

## Що і чому?

JSON - це формат обміну даними. Використовуємо його через легкість інтеграції з веб-api та міжмовної обміні.

## Як це зробити:

```Lua
-- Встановлюємо модуль 'dkjson'
local json = require("dkjson")

-- Серіалізація об'єкта Lua у рядок JSON
local data = { name = "Oleksiy", age = 29, programmer = true }
local json_string = json.encode(data)
print(json_string)

-- Десеріалізація рядка JSON назад у об'єкт Lua
local decoded_data = json.decode(json_string)
print(decoded_data.name) -- Oleksiy
```

## Занурення в тему

JSON (JavaScript Object Notation) народився з JavaScript, але став універсальним. Бібліотеки як `dkjson` для Lua вирішують завдання серіалізації/десеріалізації. До альтернатив відносяться XML та YAML, але JSON популярніший через простоту. Реалізація зазвичай базується на розборі рядка і створенні відповідних структур даних.

## Дивіться також

- Lua-users wiki про роботу з JSON: http://lua-users.org/wiki/JsonModules
- Документація dkjson: http://dkolf.de/src/dkjson-lua.fsl/home
- JSON специфікація: https://www.json.org/json-en.html
