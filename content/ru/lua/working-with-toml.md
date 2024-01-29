---
title:                "Работа с TOML"
date:                  2024-01-29T00:04:45.771005-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с TOML"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/lua/working-with-toml.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Работа с TOML включает в себя разбор и генерацию данных TOML (Tom's Obvious, Minimal Language - Очевидный Минималистичный Язык Тома) с использованием Lua. Программисты используют TOML для файлов конфигурации из-за его читаемости и простого синтаксиса, который легко транслируется в структуру данных.

## Как:
Во-первых, убедитесь, что в вашей среде Lua есть парсер TOML. Для этого примера мы будем использовать `lua-toml`.

```Lua
local toml = require("toml")

-- Разбор строки TOML
local toml_data = [[
title = "Пример TOML"

[owner]
name = "Том Престон-Вернер"
dob = 1979-05-27T07:32:00Z
]]

local data = toml.parse(toml_data)
print(data.title) -- "Пример TOML"

-- Генерация строки TOML
local table_data = {
  title = "Пример TOML",
  owner = {
    name = "Том Престон-Вернер",
    dob = os.time({year=1979, month=5, day=27, hour=7, min=32})
  }
}

local toml_string = toml.encode(table_data)
print(toml_string)
```

Пример вывода:
```
Пример TOML
```

## Подробнее
TOML был создан Томом Престон-Вернером в 2013 году как альтернатива другим языкам сериализации данных, таким как XML и YAML, предлагая более простой формат для представления данных конфигурации. Хотя JSON повсеместно используется, его синтаксис может быть громоздким для файлов конфигурации. TOML выделяется более ясным синтаксисом для людей, напоминая .ini файлы, но с возможностями вложения и типами данных.

Альтернативами TOML являются JSON, YAML и XML. Однако TOML специально разработан для конфигурации и, возможно, проще, чем YAML, более читаемый, чем JSON для целей конфигурации, и менее многословный, чем XML.

Реализация обработки TOML в Lua обычно требует сторонней библиотеки. Производительность и функции могут различаться от базового разбора до полной поддержки сериализации. При работе с большими файлами конфигурации или частыми операциями чтения/записи стоит учитывать производительность библиотеки и соответствие последней версии TOML.

## См. также
- Спецификация TOML: https://toml.io/en/
- Библиотека `lua-toml`: https://github.com/jonstoler/lua-toml
- Сравнение форматов сериализации данных: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
