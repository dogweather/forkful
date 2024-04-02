---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:21.178796-07:00
description: "JSON \u0438\u043B\u0438 JavaScript Object Notation - \u044D\u0442\u043E\
  \ \u043B\u0435\u0433\u043A\u043E\u0432\u0435\u0441\u043D\u044B\u0439 \u0444\u043E\
  \u0440\u043C\u0430\u0442 \u043E\u0431\u043C\u0435\u043D\u0430 \u0434\u0430\u043D\
  \u043D\u044B\u043C\u0438. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\
  \u0441\u0442\u044B \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u044E\u0442\
  \ JSON \u0434\u043B\u044F \u0445\u0440\u0430\u043D\u0435\u043D\u0438\u044F \u0438\
  \ \u043E\u0431\u043C\u0435\u043D\u0430 \u0434\u0430\u043D\u043D\u044B\u043C\u0438\
  , \u043F\u043E\u0442\u043E\u043C\u0443 \u0447\u0442\u043E \u043E\u043D\u2026"
lastmod: '2024-03-13T22:44:46.036181-06:00'
model: gpt-4-0125-preview
summary: "JSON \u0438\u043B\u0438 JavaScript Object Notation - \u044D\u0442\u043E\
  \ \u043B\u0435\u0433\u043A\u043E\u0432\u0435\u0441\u043D\u044B\u0439 \u0444\u043E\
  \u0440\u043C\u0430\u0442 \u043E\u0431\u043C\u0435\u043D\u0430 \u0434\u0430\u043D\
  \u043D\u044B\u043C\u0438. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\
  \u0441\u0442\u044B \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u044E\u0442\
  \ JSON \u0434\u043B\u044F \u0445\u0440\u0430\u043D\u0435\u043D\u0438\u044F \u0438\
  \ \u043E\u0431\u043C\u0435\u043D\u0430 \u0434\u0430\u043D\u043D\u044B\u043C\u0438\
  , \u043F\u043E\u0442\u043E\u043C\u0443 \u0447\u0442\u043E \u043E\u043D\u2026"
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 JSON"
weight: 38
---

## Что и зачем?
JSON или JavaScript Object Notation - это легковесный формат обмена данными. Программисты используют JSON для хранения и обмена данными, потому что он легко читается и пишется для людей и прост в разборе для машин.

## Как использовать:
В Ruby с JSON можно работать, используя встроенную библиотеку 'json'. Чтобы использовать её, просто добавьте 'json' в начало вашего кода.

```Ruby
require 'json'

# Конвертируем хэш Ruby в строку JSON
user = { name: "John Doe", email: "john.doe@example.com" }
json_string = user.to_json
puts json_string
# Вывод: {"name":"John Doe","email":"john.doe@example.com"}

# Разбор строки JSON в хэш Ruby
json_string = '{"name":"Jane Doe","email":"jane.doe@example.com"}'
parsed_data = JSON.parse(json_string)
puts parsed_data["name"]
# Вывод: Jane Doe
```

## Подробнее
JSON появился в начале 2000-х. Дуглас Крокфорд, его промоутер, стремился упростить обмен данными между сервером и клиентом в веб-приложениях по сравнению с XML.

Альтернативы JSON включают XML и YAML, хотя простота использования и широкая поддержка делают JSON предпочтительным форматом. Разбор JSON в Ruby эффективен, потому что библиотека 'json' построена на нативных расширениях, написанных на C, что значительно ускоряет разбор.

## См. также
- Спецификация JSON и информационный сайт: [JSON.org](https://www.json.org/json-ru.html)
- Сравнение JSON и XML: [XML vs JSON](https://www.w3schools.com/js/js_json_xml.asp)
