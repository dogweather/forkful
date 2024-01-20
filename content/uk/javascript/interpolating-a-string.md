---
title:                "Інтерполяція рядка"
html_title:           "Java: Інтерполяція рядка"
simple_title:         "Інтерполяція рядка"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Що і чому?

Інтерполяція рядків - це техніка програмування, які дозволяє вставляти змінні непосередньо в рядки. Це корисно для створення динамічних рядків без необхідності використовувати операції зчеплення.

## Як це:

Ось декілька прикладів на Javascript:

```Javascript
let ім'я = 'Іван';
console.log(`Привіт, ${ім'я}!`);  // Виведе: Привіт, Іван!
```

```Javascript
let х = 10;
let у = 20;
console.log(`Сума чисел: ${х + у}`);  // Виведе: Сума чисел: 30
```

## Поглиблений аналіз

1. Історичний контекст: Інтерполяція рядків була частиною JavaScript з ES6, тому що розробники шукали простіші способи створення динамічних рядків.

2. Альтернативи: Перед ES6, для введення змінних в рядки використовували зчеплення рядків за допомогою оператора "+". Однак, цей метод може стати заплутаним при великих рядках.

3. Інформація про реалізацію: В JavaScript інтерполяцію рядків можна виконати з допомогою template literals, обрамлених зворотними лапками. Всередині цього літералу, ${} використовується для вставки змінних.

## Див. також

- MDN Web Docs [Template literals](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)
- JavaScript.info [Strings](https://javascript.info/string)