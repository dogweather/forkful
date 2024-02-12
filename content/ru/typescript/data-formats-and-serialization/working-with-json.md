---
title:                "Работа с JSON"
aliases:
- /ru/typescript/working-with-json/
date:                  2024-01-29T00:04:24.816535-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/typescript/working-with-json.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
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
