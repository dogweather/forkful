---
title:                "Отримання поточної дати"
html_title:           "TypeScript: Отримання поточної дати"
simple_title:         "Отримання поточної дати"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Що це і чому?

Отримання поточної дати є процесом отримання поточної інформації про дату та час на комп'ютері. Програмісти часто здійснюють цю дію для того, щоб коректно працювати з датами та часом у своїх програмах.

## Як це зробити:

Існує кілька способів отримати поточну дату в TypeScript. Один з найпростіших способів - використовувати вбудований об'єкт `Date`. Для цього потрібно створити новий екземпляр `Date` та викликати методи `getDate()`, `getMonth()` та `getFullYear()`.

```TypeScript
let currentDate = new Date();
console.log(currentDate.getDate(), currentDate.getMonth() + 1, currentDate.getFullYear());
```

В цьому прикладі ми використали метод `getDate()` для отримання числа поточного місяця, `getMonth()` для отримання порядкового номера місяця та `getFullYear()` для отримання року.

## Глибше копання:

Перша версія JavaScript була випущена 1995 року і не мала вбудованого об'єкту `Date`. Тому розробники вирішили додати цей об'єкт до мови у наступних версіях.

Існує деяка кількість альтернативних бібліотек та пакетів, які можна використовувати для отримання поточної дати в TypeScript, таких як `moment.js` та `date-fns`.

Детально ознайомитися з роботою вбудованого об'єкта `Date` в TypeScript можна на [офіційній документації](https://www.typescriptlang.org/docs/handbook/standard-library.html#date). 

## Дивіться також:

- [Методи об'єкту `Date` в TypeScript](https://www.typescriptlang.org/docs/handbook/standard-library.html#date)
- [Moment.js](https://momentjs.com/)
- [date-fns](https://date-fns.org/)