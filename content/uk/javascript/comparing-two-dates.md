---
title:                "Порівняння двох дат"
html_title:           "Clojure: Порівняння двох дат"
simple_title:         "Порівняння двох дат"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Що це & Навіщо?

Порівняння двох дат це процес визначення, яка дата більша (пізніше), менша (раніше) або чи вони рівні. Програмісти роблять це для обробки та керування даними за вказаними датами. 

## Як це робити:

```Javascript
let date1 = new Date('2021-02-10');
let date2 = new Date('2022-02-10');

if(date1 > date2) {
  console.log('Дата 1 пізніша за дату 2');
} else if(date1 < date2) {
  console.log('Дата 2 пізніша за дату 1');
} else {
  console.log('Дати рівні');
}
```

Приклад вивода:

```Javascript
'Дата 2 пізніша за дату 1'
```

## Поглиблений огляд:

1. _Історичний контекст_: JavaScript, розроблений у 1995 році, з самого початку мав вбудовані об'єкти для роботи з датами і часом. Протягом років було додано багато функцій для праці з ними.

2. _Альтернативи_: Є бібліотеки, як-то Moment.js, які надають більш потужні функції роботи з датами.

3. _Деталі реалізації_: Порівняння двох дат в JavaScript відбувається автоматично з конвертацією дат у мілісекунди, що дозволяє просто використовувати оператори порівняння.

## Дивіться також:

1. [Date - JavaScript | MDN](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/Date)
2. [Moment.js](https://momentjs.com/)
3. [Understanding Date and Time in JavaScript](https://www.digitalocean.com/community/tutorials/understanding-date-and-time-in-javascript)