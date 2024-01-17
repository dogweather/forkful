---
title:                "Порівняння двох дат"
html_title:           "TypeScript: Порівняння двох дат"
simple_title:         "Порівняння двох дат"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Що & Чому?
Порівняння двох дат - це процес перевірки, чи одна дата попереду або пізніше за іншу. Це є важливою задачею для програмістів, оскільки дати є одними з основних компонентів більшості програм і можуть впливати на логіку інших функцій.

## Як:
Для порівняння двох дат в TypeScript, можна використовувати вбудований метод `Date.getTime()`, який повертає кількість мілісекунд з 1 січня 1970 року. За допомогою цього методу, можна порівняти дати за допомогою операторів більше/менше/рівно. Давайте подивимося на приклад коду:

```TypeScript
let date1 = new Date(2021, 10, 1); // Перша дата: 1 листопада 2021 року
let date2 = new Date(2021, 11, 1); // Друга дата: 1 грудня 2021 року

if (date1.getTime() > date2.getTime()) {
  console.log("Дата 1 пізніше за дату 2");
} else if (date1.getTime() < date2.getTime()) {
  console.log("Дата 1 раніше за дату 2");
} else {
  console.log("Обидві дати рівні");
}
```

В даному випадку, наша програма виведе повідомлення "Дата 1 раніше за дату 2".

## Глибоке погруження:
Існує кілька альтернативних способів порівняння дат в TypeScript. Одним з них є використання бібліотеки `moment.js`, яка містить розширені методи для роботи з датами. Також, можна скористатися модулем `Date-fns`, який надає функції для обробки та форматування дат.

Для більш точного порівняння дат, можна використовувати не тільки метод `Date.getTime()`, але й методи `Date.getFullYear()`, `Date.getMonth()` та інші. Це дозволить порівняти додаткові атрибути дат, наприклад, рік чи місяць.

## Додатково:
- [Вбудовані методи для роботи з датами в TypeScript](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Документація бібліотеки moment.js](https://momentjs.com/docs/#/displaying/difference/) 
- [Огляд модуля Date-fns](https://blog.bitsrc.io/working-with-dates-in-javascripts-date-fns-bd5b187066b9)