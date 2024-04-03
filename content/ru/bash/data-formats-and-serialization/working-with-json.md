---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:12.047020-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: ."
lastmod: '2024-03-13T22:44:45.408750-06:00'
model: gpt-4-0125-preview
summary: .
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 JSON"
weight: 38
---

## Как это сделать:
```Bash
# Разбор JSON с помощью 'jq':
echo '{"name": "John", "age": 31, "city": "New York"}' | jq '.name'
# Вывод: "John"

# Генерация JSON с использованием 'jq':
echo '{}' | jq --arg name "John" --arg city "New York" '. | .name=$name | .city=$city'
# Вывод: {"name":"John","city":"New York"}

# Чтение файла JSON и извлечение данных:
jq '.users[] | select(.id == "123")' users.json
# Предполагается, что users.json содержит соответствующую структуру данных.
```

## Подробнее
JSON (JavaScript Object Notation) был стандартизирован в начале 2000-х и быстро стал стандартом для обмена данными. В контексте Bash `jq` выделился как надежный инструмент для обработки JSON, который предоставляет DSL (специализированный язык) для запросов и манипулирования данными JSON. Альтернативы включают `jshon` и `jo`. Работа с JSON в Bash обычно включает использование внешних инструментов, таких как эти, поскольку Bash не имеет встроенных возможностей для анализа JSON.

## Смотрите также
- Руководство по `jq`: https://stedolan.github.io/jq/manual/
- Статья о JSON в Википедии: https://ru.wikipedia.org/wiki/JSON
- Руководство по написанию скриптов в Bash: https://www.gnu.org/software/bash/manual/
