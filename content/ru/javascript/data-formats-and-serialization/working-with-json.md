---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:06.974422-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0420\u0430\u0437\u0431\u043E\u0440 JSON \u0432 JavaScript."
lastmod: '2024-03-13T22:44:45.798511-06:00'
model: gpt-4-0125-preview
summary: "\u0420\u0430\u0437\u0431\u043E\u0440 JSON \u0432 JavaScript."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 JSON"
weight: 38
---

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
