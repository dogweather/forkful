---
title:                "Робота з JSON"
date:                  2024-01-19
html_title:           "Arduino: Робота з JSON"
simple_title:         "Робота з JSON"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/working-with-json.md"
---

{{< edit_this_page >}}

## Що це та Навіщо?
JSON — це формат обміну даними. Програмісти використовують його через простоту та сумісність з більшістю мов програмування.

## Як це зробити:
```TypeScript
// Defining a JSON object
const userJson: string = '{"name":"Олексій","age":30}';

// Parsing JSON to a TypeScript object
const user: { name: string; age: number } = JSON.parse(userJson);

console.log(user.name); // Олексій
console.log(user.age);  // 30

// Converting a TypeScript object to a JSON string
const newUser = { name: "Анна", age: 25 };
const newUserJson: string = JSON.stringify(newUser);

console.log(newUserJson); // {"name":"Анна","age":25}
```

## Заглиблення
JSON (JavaScript Object Notation) був створений у 2000-х Дугласом Крокфордом. Він легший за XML та легко читається людиною і машиною. Альтернативи включають YAML та XML. При роботі з JSON, TypeScript дозволяє типізувати об'єкти для більшої безпеки коду.

## Дивіться також
- [MDN WebDocs по JSON](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/JSON)
- [TypeScript Handbook](https://www.typescriptlang.org/docs/)
- [JSON.org](http://json.org/json-uk.html)
