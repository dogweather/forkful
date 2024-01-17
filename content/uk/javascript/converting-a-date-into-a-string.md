---
title:                "Перетворення дати у рядок"
html_title:           "Javascript: Перетворення дати у рядок"
simple_title:         "Перетворення дати у рядок"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Що і чому?
Конвертація дати у рядок є процесом перетворення дати в розуміння людини на роздільний формат, який можна легко зчитати та використовувати в програмах. Це особливо корисно для збереження дати у файловій системі або передачі її через мережу. Інша причина, чому програмісти роблять це, - це для того, щоб виводити дату у зручному для користувача форматі.

## Як зробити:
```Javascript
// Вхідна дата
var date = new Date();

// Використання методу toDateString() для конвертації в рядок
var dateString = date.toDateString();
console.log(dateString); // "Fri Apr 16 2021"

// Використання методу toISOString() для отримання формату ISO
var isoString = date.toISOString();
console.log(isoString); // "2021-04-16T04:00:00.000Z"
```

## Глибокий занурення:
Історично конвертація дати в рядок була досить складним процесом, оскільки різні країни та мови мали різні формати для виведення дати. Тому було розроблено стандартизовані формати, такі як ISO 8601, які використовуються до цього дня. Існують також альтернативні методи конвертації дати, наприклад, використання бібліотеки moment.js. У Javascript, є багато методів для отримання різних форматів дати, таких як toLocaleDateString() та toTimeString(), які можуть бути корисними в різних ситуаціях.

## Дивіться також:
- [MDN Javascript Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [MDN Date toDateString()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toDateString)
- [MDN Date toISOString()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toISOString)
- [ISO 8601](https://www.iso.org/iso-8601-date-and-time-format.html)
- [Moment.js](https://momentjs.com/)