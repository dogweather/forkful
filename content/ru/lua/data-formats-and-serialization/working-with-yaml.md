---
title:                "Работа с YAML"
aliases:
- /ru/lua/working-with-yaml.md
date:                  2024-01-29T00:05:18.422818-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/lua/working-with-yaml.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?
YAML - это формат сериализации данных, удобный для чтения и записи человеком. Программисты используют его для файлов конфигурации, обмена данными между языками и структурированного хранения данных.

## Как это сделать:

В Lua нет встроенной поддержки для YAML, но вы можете использовать библиотеку, такую как `lyaml`. Установите её, используя команду `luarocks install lyaml`. Вот как разобрать YAML:

```Lua
local lyaml = require('lyaml')

-- Пример данных YAML в виде строки
local yaml_data = [[
- name: Джон Доу
  age: 29
- name: Джейн Смит
  age: 42
]]

-- Разбор строки YAML в таблицу Lua
local parsed_data = lyaml.load(yaml_data)

-- Доступ к данным
for i, person in ipairs(parsed_data) do
  print(person.name, person.age)
end
```

Пример вывода:
```
Джон Доу 29
Джейн Смит 42
```

Теперь давайте сгенерируем некоторый YAML из таблицы Lua:

```Lua
local lyaml = require('lyaml')

-- Пример таблицы Lua
local people = {
  { name = "Джон Доу", age = 29 },
  { name = "Джейн Смит", age = 42 }
}

-- Генерация YAML из таблицы Lua
local yaml_output = lyaml.dump(people)

print(yaml_output)
```

Пример вывода YAML:
```
- age: 29
  name: Джон Доу
- age: 42
  name: Джейн Смит
```

## Подробнее

YAML, расшифровывающийся как "YAML Ain't Markup Language" (YAML - это не язык разметки), появился в начале 2000-х как дружественный стандарт сериализации данных. Он менее многословен, чем XML и JSON, что делает его популярным для файлов конфигурации. К альтернативам относятся JSON, XML и TOML. Реализация на Lua в основном зависит от внешних библиотек, таких как `lyaml`, которая использует libYAML для разбора и создания YAML. При использовании YAML с Lua помните, что таблицы не имеют внутреннего порядка, поэтому списки в YAML становятся массивами, но словари (пары ключ-значение) могут не сохранить порядок.

## Смотрите также

- Официальный сайт YAML: https://yaml.org
- Библиотека `lyaml` на GitHub: https://github.com/gvvaughan/lyaml
- Пакет LuaRocks для `lyaml`: https://luarocks.org/modules/gvvaughan/lyaml
