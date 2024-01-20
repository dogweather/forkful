---
title:                "Робота з json"
html_title:           "TypeScript: Робота з json"
simple_title:         "Робота з json"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/working-with-json.md"
---

{{< edit_this_page >}}

## Що і чому?

Робота з JSON - це процес, який використовується програмістами для обробки і передачі даних у форматі JSON. JSON - це спосіб зберігання і передавання даних, який є стандартом для багатьох веб-додатків. Програмісти використовують JSON, щоб забезпечити ефективну обробку та передачу даних.

## Як зробити:

```TypeScript
const exampleJSON = {"name": "John", "age": 25}; // створення об'єкта JSON
console.log(exampleJSON); // виведення об'єкта у консоль
```
Виведення: {"name": "John", "age": 25}

```TypeScript
const jsonString = JSON.stringify(exampleJSON); // перетворення об'єкта у JSON рядок
console.log(jsonString); // виведення рядка у консоль
```
Виведення: {"name": "John", "age": 25}

```TypeScript
const newJSON = JSON.parse(jsonString); // розбор рядка у JSON об'єкт
console.log(newJSON); // виведення об'єкта у консоль
```
Виведення: { name: "John", age: 25 }

## Заглиблення:

Історичний контекст: JSON був створений Дугласом Крокфордом у 2000 році як спрощений формат для обміну даними на веб-сторінках. Він швидко став популярним через свою легкість використання та ефективну передачу даних.

Альтернативи: Існують інші формати для обміну даними, такі як XML та CSV, але JSON зараз є стандартом для більшості веб-додатків.

Деталі реалізації: TypeScript має вбудовану підтримку для роботи з JSON, що робить його популярним вибором для програмістів. Використовуючи вбудовані функції, такі як `JSON.stringify()` та `JSON.parse()`, можна легко перетворювати дані у формат JSON та назад.

## Дивіться також:

- [Документація по JSON в TypeScript](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-4-1.html#-feat-recursive-conditional-types)
- [Стандарт JSON](https://www.json.org/json-en.html)