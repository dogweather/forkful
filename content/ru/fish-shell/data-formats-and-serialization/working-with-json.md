---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:58.966569-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: ."
lastmod: '2024-03-13T22:44:45.878377-06:00'
model: gpt-4-0125-preview
summary: .
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 JSON"
weight: 38
---

## Как это сделать:
```Fish Shell
# Разбор JSON из строки с помощью `jq`
echo '{"name": "Fish", "type": "Shell"}' | jq '.'

# Получение значения определенного ключа
echo '{"name": "Fish", "type": "Shell"}' | jq '.name'

# Вывод:
# "Fish"

# Обновление значения и вывод новой строки JSON
echo '{"name": "Fish", "type": "Shell"}' | jq '.type = "Command Line Interface"'

# Красивая печать JSON из файла
cat config.json | jq '.'
```

## Глубокое погружение
JSON, стандартизированный в начале 2000-х, имеет корни в литералах объектов JavaScript. Он быстро заменил XML для многих задач благодаря своему легковесному синтаксису и прямому отображению на структуры данных. Существуют альтернативы, такие как YAML и TOML, но повсеместное использование JSON делает его дефолтным выбором во многих сценариях. Работа с JSON в Fish требует инструментов вроде `jq`, поскольку сам по себе Fish не предназначен для интенсивной работы с данными. Исторически, Unix-оболочки используют внешние инструменты для конкретных задач, и Fish следует этой философии.

## Смотрите также
- Руководство по `jq`: https://stedolan.github.io/jq/manual/
- Документация Fish Shell: https://fishshell.com/docs/current/index.html
- Спецификация JSON: https://www.json.org/json-en.html
