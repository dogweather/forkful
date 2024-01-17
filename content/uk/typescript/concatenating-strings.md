---
title:                "Об'єднання рядків"
html_title:           "TypeScript: Об'єднання рядків"
simple_title:         "Об'єднання рядків"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Що & Чому?
Конкатенація рядків - це процес об'єднання декількох окремих рядків в один. Це часто використовується програмістами для створення складніших рядків або виведення повідомлень користувачам.

## Як?
```TypeScript
let firstName = "John";
let lastName = "Doe";
let fullName = firstName + lastName;

console.log(fullName); // Output: JohnDoe
```

```TypeScript
let str1 = "Hello";
let str2 = "world";
let greeting = str1 + " " + str2;

console.log(greeting); // Output: Hello world
```

## Поглиблене вивчення
Конкатенація рядків має своє коріння в C-подібних мовах програмування, але зараз є популярним підходом у багатьох інших мовах. Існують альтернативні методи, такі як використання шаблонних рядків або функцій для конкатенації. У TypeScript, конкатенація рядків можна виконувати за допомогою оператора "+" або методу "concat()". При цьому необхідно звернути увагу на типи даних, оскільки конкатенація з числами може призводити до небажаних результатів.

## Дивіться також
- [Шаблонні рядки в TypeScript](https://www.typescriptlang.org/docs/handbook/template-literals.html)
- [Метод concat() в JavaScript](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/String/concat)