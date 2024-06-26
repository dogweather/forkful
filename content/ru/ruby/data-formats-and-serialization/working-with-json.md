---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:21.178796-07:00
description: "\u041A\u0430\u043A \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\
  \u0430\u0442\u044C: \u0412 Ruby \u0441 JSON \u043C\u043E\u0436\u043D\u043E \u0440\
  \u0430\u0431\u043E\u0442\u0430\u0442\u044C, \u0438\u0441\u043F\u043E\u043B\u044C\
  \u0437\u0443\u044F \u0432\u0441\u0442\u0440\u043E\u0435\u043D\u043D\u0443\u044E\
  \ \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0443 'json'. \u0427\u0442\
  \u043E\u0431\u044B \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u0442\
  \u044C \u0435\u0451, \u043F\u0440\u043E\u0441\u0442\u043E \u0434\u043E\u0431\u0430\
  \u0432\u044C\u0442\u0435 'json' \u0432 \u043D\u0430\u0447\u0430\u043B\u043E \u0432\
  \u0430\u0448\u0435\u0433\u043E \u043A\u043E\u0434\u0430."
lastmod: '2024-03-13T22:44:46.036181-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Ruby \u0441 JSON \u043C\u043E\u0436\u043D\u043E \u0440\u0430\u0431\
  \u043E\u0442\u0430\u0442\u044C, \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\
  \u044F \u0432\u0441\u0442\u0440\u043E\u0435\u043D\u043D\u0443\u044E \u0431\u0438\
  \u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0443 'json'."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 JSON"
weight: 38
---

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
