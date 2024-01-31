---
title:                "Отримання поточної дати"
date:                  2024-01-20T15:15:38.114271-07:00
simple_title:         "Отримання поточної дати"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Що і чому?
Отримання поточної дати — це процес витягу інформації про теперішній день, місяць та рік. Програмісти роблять це для логування, засвідчення часу або призначення динамічних дат в їх програмах.

## Як це зробити:
```Javascript
const now = new Date();
console.log(now); // Виводить поточну дату і час

console.log(now.getFullYear()); // Виводить рік
console.log(now.getMonth() + 1); // Виводить місяць (з нуля)
console.log(now.getDate()); // Виводить число
console.log(now.getDay()); // Виводить день тижня (з нуля)
console.log(now.getHours()); // Виводить годину
console.log(now.getMinutes()); // Виводить хвилини
console.log(now.getSeconds()); // Виводить секунди
```
Приклад виводу для `console.log(now)`:
```
Tue Mar 14 2023 17:45:07 GMT+0200 (Eastern European Standard Time)
```

## Поглиблений розбір:
До впровадження `Date` об'єкту в JavaScript, отримання дати було складнішим. Від ES5, інтерфейс став стабільнішим і легшим у використанні. 

Альтернативами є бібліотеки як Moment.js, Date-fns чи Luxon, що пропонують більший функціонал при роботі з датами, але для базових задач нативний `Date` зазвичай достатньо.

Щодо реалізації, `Date` в JavaScript використовує часовий пояс користувача за замовчуванням для виводу дат і часу. Це може створювати певні проблеми при праці з часовими поясами, особливо у веб-додатках, що є глобальними.

## Додатково:
- MDN Web Docs по Date: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date
- Moment.js: https://momentjs.com/
- Date-fns: https://date-fns.org/
- Luxon: https://moment.github.io/luxon/
