---
title:                "Отримання поточної дати"
date:                  2024-01-20T15:17:57.175830-07:00
html_title:           "Bash: Отримання поточної дати"
simple_title:         "Отримання поточної дати"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Що і чому?

Отримання поточної дати — це процес визначення того, який зараз час і дата. Програмісти це роблять для логування, зведення часових міток, функціональності нагадувань чи валідації дати.

## Як це зробити:

```TypeScript
// Отримання поточної дати та часу
const now = new Date();

// Виведення поточної дати і часу в консоль
console.log(now.toString()); // "Mon Mar 15 2023 08:21:00 GMT+0200 (Eastern European Standard Time)"

// Виведення у зручному для людини форматі
console.log(now.toLocaleDateString()); // "15/03/2023"

// Виведення часу
console.log(now.toLocaleTimeString()); // "08:21:00"
```

## Поглиблений огляд

В JavaScript, з якого TypeScript бере свою основу, клас `Date` з'явився ще з перших версій мови. Методи `Date` дають можливість отримувати і маніпулювати датами і часом в залежності від часового поясу користувача.

Альтернативою є бібліотеки, такі як Moment.js або date-fns, які пропонують більше функціональностей і кращу локалізацію, але і більший розмір бібліотек. ECMAScript (стандарт, на якому базується JavaScript) продовжує вдосконалювати роботу із датами, зокрема Temporal API обіцяє вирішити багато існуючих проблем.

Цікавості про роботу з датою включають розуміння часових поясів і літнього часу, котрі можуть призводити до помилок у розрахунках. TypeScript використовує ті ж самі методи та об'єкти, що й JavaScript, тому важливо пам'ятати про такі моменти при роботі з датами.

## Дивіться також

- [Документація MDN про Date](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Temporal API – ECMAScript Proposal](https://tc39.es/proposal-temporal/docs/index.html)
- [Бібліотека date-fns](https://date-fns.org/)
- [Бібліотека Moment.js](https://momentjs.com/)
