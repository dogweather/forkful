---
title:                "Робота з json"
html_title:           "Lua: Робота з json"
simple_title:         "Робота з json"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/lua/working-with-json.md"
---

{{< edit_this_page >}}

# Що це і чому?
JSON (або JavaScript об'єктнотекстовий формат) - це формат даних, який використовується для зберігання та обміну інформацією між різними програмами або платформами. Програмісти використовують JSON для швидкого і зручного передавання даних між різними додатками та серверами.

# Як це зробити:
```Lua
-- Конвертація Lua таблиці в JSON рядок:
local json = require("json")
local table = { name = "John", age = 30, city = "Kyiv" }
local jsonString = json.encode(table)
print(jsonString) -- Виводить: {"name": "John", "age": 30, "city": "Kyiv"}

-- Конвертація JSON рядка в Lua таблицю:
local json = require("json")
local jsonString = '{"name": "John", "age": 30, "city": "Kyiv"}'
local table = json.decode(jsonString)
print(table.name) -- Виводить: John
```

# Подробиці:
JSON був розроблений для спрощення зберігання та передавання даних між веб-додатками. Він має простий синтаксис, який легко читати і розуміти людям та комп'ютерам. JSON є альтернативою іншим форматам даних, таким як XML або YAML. В Lua є об'єктна бібліотека за замовчуванням - ```json```, але також є багато сторонніх бібліотек для роботи з JSON, які можна знайти в Інтернеті.

# Дивіться також:
- Документація Lua з прикладами роботи з JSON: https://www.lua.org/pil/ (англійською)
- Огляд бібліотеки ```json4lua``` для роботи з JSON: https://github.com/craigmj/json4lua (англійською)