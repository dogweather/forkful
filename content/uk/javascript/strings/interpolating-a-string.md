---
date: 2024-01-20 17:51:22.123991-07:00
description: "\u0406\u043D\u0442\u0435\u0440\u043F\u043E\u043B\u044F\u0446\u0456\u044F\
  \ \u0440\u044F\u0434\u043A\u0456\u0432 \u0434\u043E\u0437\u0432\u043E\u043B\u044F\
  \u0454 \u0432\u0441\u0442\u0430\u0432\u043B\u044F\u0442\u0438 \u0437\u043C\u0456\
  \u043D\u043D\u0456 \u0442\u0430 \u0432\u0438\u0440\u0430\u0437\u0438 \u0432 \u043B\
  \u0456\u0442\u0435\u0440\u0430\u043B\u0438 \u0448\u0430\u0431\u043B\u043E\u043D\u0443\
  , \u0449\u043E \u043F\u043E\u043B\u0435\u0433\u0448\u0443\u0454 \u0441\u0442\u0432\
  \u043E\u0440\u0435\u043D\u043D\u044F \u0434\u0438\u043D\u0430\u043C\u0456\u0447\u043D\
  \u0438\u0445 \u0440\u044F\u0434\u043A\u0456\u0432. \u041F\u0440\u043E\u0433\u0440\
  \u0430\u043C\u0456\u0441\u0442\u0438 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\
  \u043E\u0432\u0443\u044E\u0442\u044C\u2026"
lastmod: '2024-03-13T22:44:49.968511-06:00'
model: gpt-4-1106-preview
summary: "\u0406\u043D\u0442\u0435\u0440\u043F\u043E\u043B\u044F\u0446\u0456\u044F\
  \ \u0440\u044F\u0434\u043A\u0456\u0432 \u0434\u043E\u0437\u0432\u043E\u043B\u044F\
  \u0454 \u0432\u0441\u0442\u0430\u0432\u043B\u044F\u0442\u0438 \u0437\u043C\u0456\
  \u043D\u043D\u0456 \u0442\u0430 \u0432\u0438\u0440\u0430\u0437\u0438 \u0432 \u043B\
  \u0456\u0442\u0435\u0440\u0430\u043B\u0438 \u0448\u0430\u0431\u043B\u043E\u043D\u0443\
  , \u0449\u043E \u043F\u043E\u043B\u0435\u0433\u0448\u0443\u0454 \u0441\u0442\u0432\
  \u043E\u0440\u0435\u043D\u043D\u044F \u0434\u0438\u043D\u0430\u043C\u0456\u0447\u043D\
  \u0438\u0445 \u0440\u044F\u0434\u043A\u0456\u0432. \u041F\u0440\u043E\u0433\u0440\
  \u0430\u043C\u0456\u0441\u0442\u0438 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\
  \u043E\u0432\u0443\u044E\u0442\u044C\u2026"
title: "\u0406\u043D\u0442\u0435\u0440\u043F\u043E\u043B\u044F\u0446\u0456\u044F \u0440\
  \u044F\u0434\u043A\u0456\u0432"
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
