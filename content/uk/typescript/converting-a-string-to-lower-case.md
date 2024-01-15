---
title:                "Зміна рядка на нижній регістр"
html_title:           "TypeScript: Зміна рядка на нижній регістр"
simple_title:         "Зміна рядка на нижній регістр"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## З чого почати

Веб-розробка сьогодні підкорила світ. І TypeScript стає все популярнішим мовою програмування серед розробників. TypeScript не лише збільшує продуктивність і підтримує якість коду, але також має велику базу функцій та методів, що полегшують розробку.

Серед таких функцій є й можливість перетворення рядків на нижній регістр. В цій статті ми детальніше розглянемо, як саме це робить TypeScript та чому це може бути корисно для розробників.

## Як це зробити

Для того, щоб перетворити рядок на нижній регістр, ми можемо використовувати вбудовану функцію `toLowerCase()`. Для цього просто передаємо потрібний рядок у дужках і отримуємо відформатований рядок на виході.

```TypeScript
const name = "John";
console.log(name.toLowerCase());
// output: john
```

Це дуже зручно, коли нам потрібно порівняти два рядки, але не хочемо враховувати великі та маленькі літери.

```TypeScript
const name1 = "John";
const name2 = "john";

if (name1.toLowerCase() === name2.toLowerCase()) {
  console.log("Names are equal.");
}
// output: Names are equal.
```

Ця функція також може бути корисною для перетворення користувацького вводу на нижній регістр, щоб забезпечити однаковість даних та уникнути помилок.

## Глибоке занурення

Хоча функція `toLowerCase()` досить проста, вона дозволяє нам здійснити перетворення рядка на нижній регістр, не маючи писати багаточисельні рядки коду.

Ця функція враховує особливості різних мов та символів та правильно перетворює рядок, забезпечуючи консистентність у роботі з даними.

## Дивіться також

- [Документація TypeScript](https://www.typescriptlang.org/docs/home.html)
- [Розробка веб-додатків з TypeScript](https://developer.mozilla.org/uk/docs/Learn/Tools_and_testing/Client-side_JavaScript_frameworks/React_with_TypeScript)
- [Основи програмування на TypeScript](https://www.java.com/uk/what-is-ts-programming/)