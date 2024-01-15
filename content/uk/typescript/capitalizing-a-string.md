---
title:                "Капіталізація рядка"
html_title:           "TypeScript: Капіталізація рядка"
simple_title:         "Капіталізація рядка"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Чому 
Інколи нам потрібно змінити першу літеру рядка на велику букву. Це корисно для коректного виводу на екран або для виконання певних операцій над текстом.

## Як
```TypeScript
function capitalizeString(str: string): string {
  return str.charAt(0).toUpperCase() + str.slice(1);
}

const input = "typescript";
console.log(capitalizeString(input));
// Output: Typescript
```
Цей приклад показує як змінити першу літеру рядка на велику за допомогою вбудованої функції `toUpperCase()` та методу `slice()` для отримання підрядка після першої літери.

### Використання регулярних виразів
```TypeScript
function capitalizeString(str: string): string {
  return str.replace(/\b\w/, (c) => c.toUpperCase());
}

// Or with ES6 syntax
const capitalizeString = (str: string): string => {
  return str.replace(/\b\w/, (c) => c.toUpperCase());
};

const input = "javascript";
console.log(capitalizeString(input));
// Output: Javascript
```
У цьому прикладі ми використовуємо метод `replace()` та регулярний вираз `\b\w`, який означає першу літеру кожного слова у рядку. Функція `toUpperCase()` виконує зміну першої літери на велику.

## Deep Dive
У TypeScript є декілька вбудованих методів для роботи зі строками, які можна використовувати для капіталізації рядків.
- `toUpperCase()` - змінює всі букви рядка на великі.
- `toLowerCase()` - змінює всі букви рядка на малі.
- `charAt()` - повертає символ за заданим індексом у рядку.
- `slice()` - повертає підрядок рядка, починаючи з вказаного індексу.

У TypeScript також є можливість використовувати функціональне програмування для капіталізації рядків. Це дозволяє використовувати зворотній виклик (callback) для зміни кожної букви у рядку.

## See Also
- [TypeScript документація по рядках](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [Регулярні вирази у TypeScript](https://www.typescriptlang.org/docs/handbook/declaration-files/do-s-and-don-ts.html#regular-expressions)