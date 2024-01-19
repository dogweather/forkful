---
title:                "Аналіз дати з рядка"
html_title:           "C++: Аналіз дати з рядка"
simple_title:         "Аналіз дати з рядка"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Що і чому?
Розбір дати з рядка - це процес, подібний до перетворення тексту на представлення дати та часу, що може розуміти програма. Програмісти це роблять, щоб машина могла керувати даними про дату/час як числами, а не рядками.

## Як це зробити:
```Javascript
let dateString = "2020-01-15T18:00:00.000Z"; // Це рядок дати ISO

let date = new Date(dateString); // Конвертація рядка в об'єкт Date

console.log(date); // Виводить "Wed Jan 15 2020 13:00:00 GMT-0500 (Eastern Standard Time)"
```
Вищенаведений код приклад того, як зробити парсинг рядка ISO в об'єкт JavaScript Date.

## Поглиблено:
Розбір дати з рядка був необхідним ще з перших версій мови JavaScript. JavaScript розвивався, і з появою ES2015 було введено багато поліпшень. 

1. **Історичний контекст**: На початкових стадіях JavaScript, розробники часто керували датами та часом, конвертуючи дати в мілісекунди з 1 січня 1970 року. Це забезпечує універсальність при роботі з датами.

2. **Альтернативи**: Є різні бібліотеки, такі як Moment.js або Day.js, які пропонують набагато більше функціональності для роботи з датами.

3. **Деталі реалізації**: Парсинг рядка-дати за допомогою вбудованих засобів JavaScript залежить від браузера. На противагу цьому, бібліотеки для роботи з датами, як правило, працюють однаково в усіх браузерах.

## Див. також
1. [Date - JavaScript | MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
2. [Moment.js](https://momentjs.com/)
3. [Day.js](https://day.js.org/)