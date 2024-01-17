---
title:                "Порівняння двох дат"
html_title:           "Javascript: Порівняння двох дат"
simple_title:         "Порівняння двох дат"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

Що & Чому?
Порівняння двох дат є процесом, під час якого ми порівнюємо одну дату з іншою, для визначення, яка з них раніше або пізніше. Програмісти виконують це для багатьох причин, включаючи встановлення порядку подій, обробки даних та створення логіки програм.

Як це зробити:
```Javascript
const date1 = new Date('2021-01-01');
const date2 = new Date('2021-01-02');

if (date1 > date2) {
  console.log('Date 1 is later than date 2');
} else if (date1 < date2) {
  console.log('Date 1 is earlier than date 2');
} else {
  console.log('Date 1 is equal to date 2');
}
```
Виведе "Date 1 is earlier than date 2".

Глибоке погруження:
Історичний контекст: Порівняння дат активно використовувалося в комп'ютерах вже з початку їх винайдення. Раніше для цього використовувались числові представлення дат, однак із появою різних форматів дати, таких як ISO-8601, стало популярніше використовувати вбудовані функції для порівняння.

Альтернативи: Окрім використання вбудованих функцій у Javascript, існують також бібліотеки, які спеціалізуються на порівнянні дат, наприклад Moment.js.

Деталі реалізації: В Javascript існує вбудована функція `Date()` для створення об'єкта дати. Ця функція приймає різні типи вхідних даних, які потім перетворюються у датові об'єкти, наприклад строки, числа або дати. Можна також використовувати методи `getFullYear()`, `getMonth()`, `getDate()` для отримання частин дати, які можна порівнювати між собою.

Дивіться також:
- [Документація MDN про вбудований об'єкт дати у Javascript](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js бібліотека](https://momentjs.com/)
- [Стаття про порівняння дат з використанням If/Else у Javascript](https://www.delftstack.com/uk/howto/javascript/javascript-compare-dates/)