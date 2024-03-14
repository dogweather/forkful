---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:24.816535-07:00
description: "JSON (JavaScript Object Notation) - \u044D\u0442\u043E \u043B\u0435\u0433\
  \u043A\u043E\u0432\u0435\u0441\u043D\u044B\u0439 \u0444\u043E\u0440\u043C\u0430\u0442\
  \ \u0434\u0430\u043D\u043D\u044B\u0445 \u0434\u043B\u044F \u0445\u0440\u0430\u043D\
  \u0435\u043D\u0438\u044F \u0438 \u043F\u0435\u0440\u0435\u0434\u0430\u0447\u0438\
  \ \u0434\u0430\u043D\u043D\u044B\u0445. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\
  \u043C\u0438\u0441\u0442\u044B \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u044E\
  \u0442 \u0435\u0433\u043E, \u043F\u043E\u0442\u043E\u043C\u0443 \u0447\u0442\u043E\
  \ \u043E\u043D \u0443\u0434\u043E\u0431\u043E\u0447\u0438\u0442\u0430\u0435\u043C\
  ,\u2026"
lastmod: '2024-03-13T22:44:44.627736-06:00'
model: gpt-4-0125-preview
summary: "JSON (JavaScript Object Notation) - \u044D\u0442\u043E \u043B\u0435\u0433\
  \u043A\u043E\u0432\u0435\u0441\u043D\u044B\u0439 \u0444\u043E\u0440\u043C\u0430\u0442\
  \ \u0434\u0430\u043D\u043D\u044B\u0445 \u0434\u043B\u044F \u0445\u0440\u0430\u043D\
  \u0435\u043D\u0438\u044F \u0438 \u043F\u0435\u0440\u0435\u0434\u0430\u0447\u0438\
  \ \u0434\u0430\u043D\u043D\u044B\u0445. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\
  \u043C\u0438\u0441\u0442\u044B \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u044E\
  \u0442 \u0435\u0433\u043E, \u043F\u043E\u0442\u043E\u043C\u0443 \u0447\u0442\u043E\
  \ \u043E\u043D \u0443\u0434\u043E\u0431\u043E\u0447\u0438\u0442\u0430\u0435\u043C\
  ,\u2026"
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 JSON"
---

{{< edit_this_page >}}

## Что и Почему?

JSON (JavaScript Object Notation) - это легковесный формат данных для хранения и передачи данных. Программисты используют его, потому что он удобочитаем, легко анализируется и широко используется в веб-API и конфигурациях.

## Как это сделать:

**Разбор JSON:**

```TypeScript
const jsonString = '{"name":"John", "age":30, "city":"New York"}';
let user = JSON.parse(jsonString);
console.log(user.name); // John
```

**Преобразование объектов JavaScript в строки JSON:**

```TypeScript
const userObject = { name: 'Jane', age: 25, city: 'Los Angeles' };
let jsonOutput = JSON.stringify(userObject);
console.log(jsonOutput); // {"name":"Jane","age":25,"city":"Los Angeles"}
```

**Объявления типов:**

```TypeScript
type User = {
  name: string;
  age: number;
  city: string;
};

const userJson = '{"name":"Jack", "age":28, "city":"Chicago"}';
let user: User = JSON.parse(userJson);
console.log(user.city); // Chicago
```

## Углубленный анализ

JSON возник на основе JavaScript, но теперь не зависит от языка; он стал основным способом обмена данными, заменив XML благодаря своей простоте. Хотя JSON изначально не накладывает ограничений на типы (что является основной задачей TypeScript), TypeScript позволяет вам определить типы для гарантии соответствия структуры JSON вашим ожиданиям. И хотя JSON является королем для API, для файлов конфигурации некоторые предпочитают YAML, который более удобочитаем. Под капотом, когда в TypeScript вызываются `JSON.parse()` или `JSON.stringify()`, на самом деле вызываются функции JSON движка JavaScript; основная роль TypeScript здесь - обеспечить безопасность типов.

## Смотрите также

- [JSON.org](https://www.json.org/json-en.html): Официальная документация JSON.
- [MDN - Работа с JSON](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Objects/JSON): Добрый старый MDN предоставляет общий обзор и примеры использования.
