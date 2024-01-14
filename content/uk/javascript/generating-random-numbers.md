---
title:                "Javascript: Створення випадкових чисел"
programming_language: "Javascript"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Чому
Генерація випадкових чисел може бути корисною при розв'язуванні різних завдань, таких як шифрування даних або створення випадкових об'єктів у гральних програмах.

## Як
Використовуючи вбудовану функцію `Math.random()`, ми можемо згенерувати випадкове число від 0 до 1. Для отримання випадкового числа у певному діапазоні, скористайтеся наступним кодом:

```Javascript
// генерація випадкового числа від 1 до 10
let randomNumber = Math.floor(Math.random() * 10 + 1);
console.log(randomNumber); // виведе випадкове число від 1 до 10
```

Для генерації випадкового цілого числа використовуйте функцію `Math.floor()`, яка округлює результат до найближчого меншого цілого числа. Щоб випадкове число було включно до заданого діапазону, додаємо 1 до результату.

## Глибока аналітика
Хоча функція `Math.random()` є швидкою та простою для генерації випадкових чисел, вона не є повністю випадковою. Відомо, що вона генерує послідовності чисел, які повторюються через певну кількість викликів. Для більш безпечних та випадкових результатів, можна скористатися зовнішніми бібліотеками, такими як `crypto.getRandomValues()`.

## Дивіться Також
- [Math.random()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [Generating cryptographically secure random numbers in Node.js](https://curtisfornadley.medium.com/generating-cryptographically-secure-random-numbers-in-node-js-e220f39f30da)
- [Introduction to Random Number Generators in JavaScript](https://www.freecodecamp.org/news/introduction-to-random-number-generators-in-javascript/)
- [crypto.getRandomValues()](https://developer.mozilla.org/en-US/docs/Web/API/Crypto/getRandomValues)