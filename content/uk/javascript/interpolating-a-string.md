---
title:                "Інтерполяція рядків"
date:                  2024-01-20T17:51:22.123991-07:00
model:                 gpt-4-1106-preview
simple_title:         "Інтерполяція рядків"

category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Що це таке і навіщо?
Інтерполяція рядків дозволяє вставляти змінні та вирази в літерали шаблону, що полегшує створення динамічних рядків. Програмісти використовують інтерполяцію, щоб зробити код чистішим та зручнішим для читання.

## Як це робити:
```Javascript
let name = 'Віталій';
let greeting = `Привіт, ${name}!`;
console.log(greeting); // Виведе: Привіт, Віталій!

let price = 19.99;
let taxRate = 0.2;
let total = `Загальна сума: ${price + (price * taxRate)} грн.`;
console.log(total); // Виведе: Загальна сума: 23.988 грн.
```

## Поглиблений огляд:
Інтерполяція рядків у JavaScript стала суттєво простішою з появою шаблонних літералів у ES6 (ECMAScript 2015). Раніше програмісти мали складати рядки використовуючи конкатенацію, що змушувало їх вставляти змінні через "+", що робило код менш читабельним.

```Javascript
// Конкатенація у старих версіях JavaScript
let message = 'Привіт, ' + name + '!';
```

Інші мови також мають свої форми інтерполяції, наприклад, Python використовує f-рядки, а Ruby - інтерполяцію всередині рядків з "#{}".

Щодо імплементації, подумайте про те, що шаблонні літерали в JavaScript використовують зворотні кавички (\`) і дозволяють вставляти вирази всередину ${}, які автоматично будуть приведені до рядкового типу.

## Дивіться також:
- [MDN Template literals (Template strings) documentation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)
- [ECMAScript 6: New Features: Overview and Comparison](http://es6-features.org/#StringInterpolation)
