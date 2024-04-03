---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:06.974422-07:00
description: "JSON \u0438\u043B\u0438 JavaScript Object Notation - \u044D\u0442\u043E\
  \ \u043B\u0435\u0433\u043A\u043E\u0432\u0435\u0441\u043D\u044B\u0439 \u0444\u043E\
  \u0440\u043C\u0430\u0442 \u0434\u0430\u043D\u043D\u044B\u0445 \u0434\u043B\u044F\
  \ \u0445\u0440\u0430\u043D\u0435\u043D\u0438\u044F \u0438 \u043F\u0435\u0440\u0435\
  \u0434\u0430\u0447\u0438 \u0434\u0430\u043D\u043D\u044B\u0445. \u041F\u0440\u043E\
  \u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0438\u0441\u043F\u043E\u043B\
  \u044C\u0437\u0443\u044E\u0442 \u0435\u0433\u043E, \u043F\u043E\u0442\u043E\u043C\
  \u0443 \u0447\u0442\u043E \u043E\u043D \u043B\u0435\u0433\u043A\u043E\u2026"
lastmod: '2024-03-13T22:44:45.798511-06:00'
model: gpt-4-0125-preview
summary: "JSON \u0438\u043B\u0438 JavaScript Object Notation - \u044D\u0442\u043E\
  \ \u043B\u0435\u0433\u043A\u043E\u0432\u0435\u0441\u043D\u044B\u0439 \u0444\u043E\
  \u0440\u043C\u0430\u0442 \u0434\u0430\u043D\u043D\u044B\u0445 \u0434\u043B\u044F\
  \ \u0445\u0440\u0430\u043D\u0435\u043D\u0438\u044F \u0438 \u043F\u0435\u0440\u0435\
  \u0434\u0430\u0447\u0438 \u0434\u0430\u043D\u043D\u044B\u0445."
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
