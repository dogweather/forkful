---
title:                "Генерація випадкових чисел"
date:                  2024-01-20T17:49:36.336653-07:00
model:                 gpt-4-1106-preview
simple_title:         "Генерація випадкових чисел"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Що і Навіщо?
Генерація випадкових чисел - це просто дістати число, яке непередбачувано змінюється. У програмуванні це робить гру більш захоплюючою, тестування - рандомізованим, а дані - анонімними.

## Як це зробити:
Згенеруймо випадкове число в JavaScript:

```javascript
// Випадкове число від 0 до 1 (не включно 1)
let randomNum = Math.random();
console.log(randomNum);

// Випадкове ціле число від 0 до 10
let randomInt = Math.floor(Math.random() * 11);
console.log(randomInt);

// Функція для генерації випадкового числа в діапазоні від min до max
function getRandomInt(min, max) {
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

// Використаємо функцію для генерації числа від 1 до 50
console.log(getRandomInt(1, 50));
```

## Поглиблений Розбір:
Історично, генерація рандомних чисел відбувалася без комп'ютерів - через кидання кісток чи монет. У JavaScript `Math.random()` генерує псевдовипадкове число – достатньо добре для більшості ситуацій, але не для криптографії. Для більш безпечної генерації криптографічно-стійких чисел використовують `crypto.getRandomValues()`. Також, для надійності ви можете використовувати бібліотеки як `Chance` чи `random-js`.

## Дивіться також:
- MDN Web Docs для [`Math.random()`](https://developer.mozilla.org/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- MDN Web Docs для [`crypto.getRandomValues()`](https://developer.mozilla.org/docs/Web/API/Crypto/getRandomValues)
- [Chance.js](http://chancejs.com/) - Генератор випадкових чисел з більшими можливостями
- [random-js](https://github.com/ckknight/random-js) - Бібліотека для випадкових чисел з генератором ентропії