---
title:                "Работа с JSON"
date:                  2024-01-29T00:04:12.047020-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/bash/working-with-json.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Работа с JSON в Bash включает в себя анализ и генерирование данных в формате JSON непосредственно из командной строки. Программисты делают это для управления конфигурациями, взаимодействия с API и обмена данными между службами из-за всеприсутствия JSON на различных платформах и языках.

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
