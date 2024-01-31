---
title:                "Написання тестів"
date:                  2024-01-19
html_title:           "Arduino: Написання тестів"
simple_title:         "Написання тестів"

category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/writing-tests.md"
---

{{< edit_this_page >}}

## Що це таке та чому?

Writing tests – це процес створення сценаріїв, які перевіряють, чи правильно працює код. Програмісти пишуть тести, аби забезпечити надійність і якість коду, виявити помилки рано і уникнути їх у майбутньому.

## Як це робити:

JavaScript-раозробники часто використовують Jest для написання тестів. Це ось як можна тестувати суму двох чисел.

```javascript
// sum.js
function sum(a, b) {
  return a + b;
}
module.exports = sum;

// sum.test.js
const sum = require('./sum');

test('adds 1 + 2 to equal 3', () => {
  expect(sum(1, 2)).toBe(3);
});

// Запуск тестів за допомогою команди:
// $ npm test

// Очікуваний вивід:
// PASS  ./sum.test.js
// ✓ adds 1 + 2 to equal 3 (5ms)
```

## Поглиблено:

Тести в JavaScript з'явилися не відразу після створення мови. Раніше код часто перевіряли вручну. З JQuery, Mocha, Jasmine та іншими фреймворками, програмісти отримали засоби для автоматизації цього процесу. Зараз Jest став свого роду стандартом через простоту та функціональність. 

## Дивіться також:

- [Jest official site](https://jestjs.io/)
- [Mocha official site](https://mochajs.org/)
- [Jasmine official site](https://jasmine.github.io/)
