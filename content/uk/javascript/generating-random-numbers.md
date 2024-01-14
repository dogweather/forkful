---
title:    "Javascript: Генерування випадкових чисел"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Чому
Згенерувати випадкові числа є важливим аспектом програмування, особливо при роботі з великими наборами даних. Випадкові числа дозволяють створити випадкові умови для тестування і виконання алгоритмів.

## Як це зробити
```Javascript
// Випадкове ціле число в діапазоні від 1 до 10
let randomNumber = Math.floor(Math.random() * 10) + 1;
console.log(randomNumber);

// Випадкове число в діапазоні від 0 до 1
let randomDecimal = Math.random();
console.log(randomDecimal);

// Випадковий елемент зі списку
let array = ['apple', 'orange', 'banana', 'grape'];
let randomItem = array[Math.floor(Math.random() * array.length)];
console.log(randomItem);
```

Вищевказані приклади демонструють використання методів ```Math.random()``` та ```Math.floor()``` для генерування випадкових цілих чисел і десяткових чисел з обмеженим діапазоном. Також це можливо зробити для випадкового елемента зі списку шляхом генерації випадкового індексу.

## Глибше поглиблення
Існує кілька методів для генерації випадкових чисел, таких як ```Math.random()```, ```Math.floor()```, ```Math.ceil()``` та ```Math.round()```. Кожен з цих методів має свої особливості і поведінку при обробці чисел. Також важливо уникати використання псевдовипадкових чисел при створенні криптографічної безпеки, оскільки ці числа можуть бути передбаченими.

## Дивись також
- [Документація MDN: Math.random()](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [Стаття: Генерація випадкових чисел в Javascript](https://medium.com/@jvalimaki/computer-science-in-javascript-games-random-7d96e9d52b44)
- [Курс: Випадкові числа в Javascript](https://www.udacity.com/course/intro-to-javascript--ud803)