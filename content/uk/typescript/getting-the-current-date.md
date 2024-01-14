---
title:                "TypeScript: Отримання поточної дати"
simple_title:         "Отримання поточної дати"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Чому

Програмування є надзвичайно корисним навичкою, особливо в сучасному технологічному світі. У цій статті ми розглянемо одну з основних задач - отримання поточної дати. Давайте почнемо!

## Як

Існує декілька способів отримати поточну дату в TypeScript. Перший спосіб - використання вбудованого об'єкта Date. Наприклад, якщо ви хочете отримати сьогоднішню дату, ви можете скористатися кодом:

```TypeScript
const today = new Date();
console.log(today);
```

Ви отримаєте вихідний результат у вигляді:

```javascript
2019-11-08T08:00:00.000Z
```

Ви також можете отримати окремі частини дати, наприклад, день, місяць або рік:

```TypeScript
const year = today.getFullYear();
const month = today.getMonth(); // Повертає 0 для січня, 1 для лютого і т.д.
const day = today.getDate();
console.log(`${day}/${month + 1}/${year}`);
```

Результат буде виглядати наступним чином:

```javascript
8/11/2019
```

Ще один спосіб отримати дату - використання бібліотеки Moment.js. Вона надає зручні інструменти для роботи з датами в TypeScript. Встановити її можна за допомогою наступної команди:

```bash
npm install moment --save
```

Після цього, ви можете використовувати бібліотеку за допомогою такого коду:

```TypeScript
import moment from "moment";
const today = moment();
console.log(today.format("DD/MM/YYYY"));
```

Результат буде аналогічний попередньому прикладу.

## Глибокий занурення

Якщо ви хочете дізнатися більше про отримання дати в TypeScript, рекомендую переглянути документацію до бібліотеки Moment.js. Також ви можете прочитати про роботу з датами в стандартній бібліотеці JavaScript.

## Дивіться також

- [Документація Moment.js](https://momentjs.com/docs/)
- [Робота з датами в JavaScript](https://www.w3schools.com/js/js_dates.asp)