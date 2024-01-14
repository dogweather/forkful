---
title:    "Javascript: Отримання поточної дати"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Чому

У цьому сучасному світі, де всі живуть за розкладом, знаходження поточної дати є дуже важливим. Це допомагає людям виконувати плани і зустрічатися у визначені дні і часи. Але чи знаєте ви, що це також можна зробити за допомогою програмування?

## Як

Використання Javascript є простим та зручним способом отримання поточної дати. Нижче ви можете побачити приклад коду та його виводу:

```Javascript
const currentDate = new Date();  
console.log(currentDate);  
```

Вивід: `Wed Sep 29 2021 17:22:10 GMT+0300 (Eastern European Summer Time)`

Цей код створює змінну `currentDate`, яка містить поточну дату та час. Це досягається за допомогою використання вбудованого об'єкта `Date` та його методу `new()`. Потім за допомогою функції `console.log()` ми можемо вивести значення змінної `currentDate` у консоль.

Також, за допомогою методів `getDate()`, `getMonth()` та `getFullYear()` ми можемо отримати окремо день, місяць та рік поточної дати.

```Javascript
const currentDate = new Date();  
const day = currentDate.getDate();  
const month = currentDate.getMonth();  
const year = currentDate.getFullYear();  

console.log(`Сьогодні ${day}.${month + 1}.${year}`);  
```

Вивід: `Сьогодні 29.9.2021`

## Глибше вдивимось

Існує багато інших методів та властивостей для отримання поточної дати в Javascript. Наприклад, з допомогою методу `getDay()` можна отримати номер поточного дня тижня, а `getHours()`, `getMinutes()`, `getSeconds()` допоможуть отримати усі необхідні дані про час.

Також існують бібліотеки, які спрощують процес роботи з датою та часом, наприклад Moment.js, Luxon та Day.js. Вони мають більш зручний та ефективний спосіб роботи з датою та часом та надають більше функціональності.

## Дивіться також

- [Приклади роботи з датою та часом в Javascript](https://www.digitalocean.com/community/tutorials/working-with-date-and-time-in-javascript-ru)
- [Бібліотека Moment.js](https://momentjs.com/)
- [Бібліотека Luxon](https://moment.github.io/luxon/)
- [Бібліотека Day.js](https://day.js.org/)