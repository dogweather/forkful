---
title:                "Робота з YAML"
html_title:           "Arduino: Робота з YAML"
simple_title:         "Робота з YAML"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/lua/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? / Що таке & Чому?
YAML - це формат серіалізації даних, легкий для людського ока. Програмісти використовують YAML через його зрозумілість та простоту інтеграції з різними мовами програмування.

## How to: / Як це зробити:
Lua не має вбудованої підтримки YAML, але ви можете використовувати зовнішні бібліотеки. Приклад з `lyaml`:

```Lua
local lyaml = require 'lyaml'
-- Читання YAML
local yaml_data = [[
- Вітаємо
- у
- YAML
]]
local table_from_yaml = lyaml.load(yaml_data)
print(table_from_yaml[1])  -- Виведе: Вітаємо

-- Запис у YAML
local table_to_yaml = { "Рік", "Місяць", "День" }
local yaml_output = lyaml.dump({table_to_yaml})
print(yaml_output)
```

Sample output / Приклад виводу:
```
Вітаємо
- Рік
- Місяць
- День
```

## Deep Dive / Занурення у деталі:
YAML (YAML Ain't Markup Language) з'явився у 2001 році як зручний формат для конфігураційних файлів. Альтернативами є JSON та XML. YAML визначається із простоти читання та відсутності зайвих символів, на відміну від XML. При роботі з YAML у Lua, важливо вибрати потрібну бібліотеку та зважати на можливі питання перетворення типів даних.

## See Also / Дивіться також:
- Офіційний сайт YAML: [YAML](https://yaml.org)
- lyaml бібліотека: [GitHub lyaml](https://github.com/gvvaughan/lyaml)
- YAML для більшої глибини специфікацій: [YAML Spec](https://yaml.org/spec/1.2/spec.html)
