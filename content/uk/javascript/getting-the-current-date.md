---
title:                "Отримання поточної дати."
html_title:           "Javascript: Отримання поточної дати."
simple_title:         "Отримання поточної дати."
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Причина

Отримання поточної дати може бути корисною операцією для багатьох програм на Javascript. Наприклад, вона може використовуватися для створення часових нагадувань, виведення поточної дати на веб-сторінці або для аналізу даних, пов'язаних з датами.

## Як

```javascript
const currentDate = new Date();

console.log(currentDate); // Виводить поточну дату та час
```

Для отримання поточної дати використовується вбудований об'єкт `Date` у Javascript. Метод `new Date()` створює новий об'єкт з поточною датою та часом. Щоб вивести це значення консолі, використовується метод `console.log()`.

Для отримання конкретної частини дати, наприклад дня, місяця або року, можна використовувати вбудовані методи об'єкта `Date`, такі як `getDate()`, `getMonth()` та `getFullYear()`. Наприклад:

```javascript
console.log(currentDate.getDate()); // Виводить поточний день
console.log(currentDate.getMonth()); // Виводить поточний місяць (починаючи з 0)
console.log(currentDate.getFullYear()); // Виводить поточний рік
```

## Глибокий занурений

Об'єкт `Date` у Javascript також зберігає час у мілісекундах, починаючи з 1 січня 1970 року. Ці мілісекунди називаються "епохою". У цьому форматі отримання поточної дати дозволяє отримати значення, більш точне, ніж просто рік, місяць та день.

Крім того, в об'єкті `Date` є методи для роботи з часовими зонами та для виконання інших дій з датами, таких як додавання або віднімання днів, місяців та років.

## Дивіться також

- [MDN - Об'єкт Date у Javascript](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Корисні функції для роботи з датами у Javascript](https://www.sitepoint.com/date-functions-javascript/)
- [W3Schools - Розбір часових зон у Javascript](https://www.w3schools.com/js/js_dates.asp)