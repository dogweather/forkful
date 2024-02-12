---
title:                "Работа с JSON"
aliases:
- /ru/javascript/working-with-json.md
date:                  2024-01-29T00:04:06.974422-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/javascript/working-with-json.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
JSON или JavaScript Object Notation - это легковесный формат данных для хранения и передачи данных. Программисты используют его, потому что он легко читается и пишется как людьми, так и машины могут быстро его анализировать и генерировать.

## Как это сделать:
Разбор JSON в JavaScript:
```javascript
const jsonString = '{"name":"John", "age":30, "city":"New York"}';
const user = JSON.parse(jsonString);
console.log(user.name); // Вывод: John
```

Преобразование объекта JavaScript в JSON:
```javascript
const user = { name: 'John', age: 30, city: 'New York' };
const jsonString = JSON.stringify(user);
console.log(jsonString); // Вывод: '{"name":"John","age":30,"city":"New York"}'
```

## Глубже
JSON был выведен из JavaScript, но теперь является форматом, независимым от языка. Существует множество альтернатив, таких как XML, но минимальный синтаксис JSON обрел популярность для API. Технически, JSON является подмножеством литеральной нотации объектов JavaScript с некоторыми отличиями, например, требует, чтобы ключи были заключены в двойные кавычки.

## Смотрите также
- MDN JSON: https://developer.mozilla.org/ru/docs/Web/JavaScript/Reference/Global_Objects/JSON
- JSON Formatter & Validator: https://jsonlint.com/
- JSON vs. XML: https://www.w3schools.com/js/js_json_xml.asp
