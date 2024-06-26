---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:18.422818-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412 Lua \u043D\u0435\u0442 \u0432\u0441\u0442\u0440\u043E\u0435\
  \u043D\u043D\u043E\u0439 \u043F\u043E\u0434\u0434\u0435\u0440\u0436\u043A\u0438\
  \ \u0434\u043B\u044F YAML, \u043D\u043E \u0432\u044B \u043C\u043E\u0436\u0435\u0442\
  \u0435 \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u0442\u044C\
  \ \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0443, \u0442\u0430\u043A\
  \u0443\u044E \u043A\u0430\u043A `lyaml`. \u0423\u0441\u0442\u0430\u043D\u043E\u0432\
  \u0438\u0442\u0435 \u0435\u0451, \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\
  \u044F \u043A\u043E\u043C\u0430\u043D\u0434\u0443\u2026"
lastmod: '2024-03-13T22:44:45.326096-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Lua \u043D\u0435\u0442 \u0432\u0441\u0442\u0440\u043E\u0435\u043D\
  \u043D\u043E\u0439 \u043F\u043E\u0434\u0434\u0435\u0440\u0436\u043A\u0438 \u0434\
  \u043B\u044F YAML, \u043D\u043E \u0432\u044B \u043C\u043E\u0436\u0435\u0442\u0435\
  \ \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u0442\u044C \u0431\
  \u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0443, \u0442\u0430\u043A\u0443\
  \u044E \u043A\u0430\u043A `lyaml`."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 YAML"
weight: 41
---

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
