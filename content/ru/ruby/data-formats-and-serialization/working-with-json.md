---
title:                "Работа с JSON"
aliases:
- /ru/ruby/working-with-json.md
date:                  2024-01-29T00:04:21.178796-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/ruby/working-with-json.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
