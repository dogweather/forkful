---
title:                "Розрахунок дати в майбутньому або минулому"
html_title:           "Javascript: Розрахунок дати в майбутньому або минулому"
simple_title:         "Розрахунок дати в майбутньому або минулому"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Що і чому?
Обчислення дат в майбутньому або минулому - це задача, яка включає в себе додавання або віднімання днів, місяців або років від конкретної дати. Це корисно для програмістів при створенні календарів, застосунків для нагадування, історичних моделей даних тощо.

## Як це зробити:
```Javascript
let now = new Date();
let future = new Date();
future.setDate(now.getDate() + 10);
console.log(`Сьогодні: ${now.toDateString()}`);
console.log(`Через 10 днів: ${future.toDateString()}`);
```

## Поглиблений огляд
Історично, обчислення дати у майбутньому або минулому було складною задачею, враховуючи високосні роки, різне кількість днів у місяцях і т.д. JavaScript забезпечує вбудовані функції для легкого руху між датами. Альтернативами для цього можуть бути бібліотеки, такі як Moment.js, які пропонують більш широкий функціонал та гнучкість.
Деталі реалізації: "new Date()" створює новий об'єкт дати із поточного моменту, а "setDate()" встановлює день місяця для зазначеної дати відповідно до місцевого часу. "getDate()" отримує день місяця для вказаної дати відповідно до місцевого чasу.

## Див. також
1. MDN Web Docs [дата](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/Date)
1. Бібліотека JavaScript для роботи з датами [Moment.js](https://momentjs.com/)