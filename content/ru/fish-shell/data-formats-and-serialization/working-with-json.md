---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:58.966569-07:00
description: "JSON (JavaScript Object Notation, \u043D\u043E\u0442\u0430\u0446\u0438\
  \u044F \u043E\u0431\u044A\u0435\u043A\u0442\u043E\u0432 JavaScript) - \u044D\u0442\
  \u043E \u0444\u043E\u0440\u043C\u0430\u0442 \u0434\u0430\u043D\u043D\u044B\u0445\
  , \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\u043C\u044B\u0439 \u0434\
  \u043B\u044F \u043F\u0440\u0435\u0434\u0441\u0442\u0430\u0432\u043B\u0435\u043D\u0438\
  \u044F \u0441\u0442\u0440\u0443\u043A\u0442\u0443\u0440\u0438\u0440\u043E\u0432\u0430\
  \u043D\u043D\u044B\u0445 \u0434\u0430\u043D\u043D\u044B\u0445. \u041F\u0440\u043E\
  \u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B\u2026"
lastmod: '2024-03-13T22:44:45.878377-06:00'
model: gpt-4-0125-preview
summary: "JSON (JavaScript Object Notation, \u043D\u043E\u0442\u0430\u0446\u0438\u044F\
  \ \u043E\u0431\u044A\u0435\u043A\u0442\u043E\u0432 JavaScript) - \u044D\u0442\u043E\
  \ \u0444\u043E\u0440\u043C\u0430\u0442 \u0434\u0430\u043D\u043D\u044B\u0445, \u0438\
  \u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\u043C\u044B\u0439 \u0434\u043B\u044F\
  \ \u043F\u0440\u0435\u0434\u0441\u0442\u0430\u0432\u043B\u0435\u043D\u0438\u044F\
  \ \u0441\u0442\u0440\u0443\u043A\u0442\u0443\u0440\u0438\u0440\u043E\u0432\u0430\
  \u043D\u043D\u044B\u0445 \u0434\u0430\u043D\u043D\u044B\u0445. \u041F\u0440\u043E\
  \u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B\u2026"
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 JSON"
---

{{< edit_this_page >}}

## Что и Почему?

JSON (JavaScript Object Notation, нотация объектов JavaScript) - это формат данных, используемый для представления структурированных данных. Программисты используют JSON, потому что он легко читается и записывается людьми, а также легко анализируется и генерируется компьютерами.

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
