---
date: 2024-01-20 17:33:32.463401-07:00
description: "\u042F\u043A \u0446\u0435 \u0440\u043E\u0431\u0438\u0442\u0438: \u041F\
  \u043E\u0440\u0456\u0432\u043D\u044F\u043D\u043D\u044F \u0434\u0432\u043E\u0445\
  \ \u0434\u0430\u0442 \u0443 JavaScript \u043C\u043E\u0436\u0435 \u0431\u0443\u0442\
  \u0438 \u043F\u0440\u043E\u0441\u0442\u0438\u043C, \u0430\u043B\u0435 \u0432\u0430\
  \u0436\u043B\u0438\u0432\u043E \u0440\u043E\u0437\u0443\u043C\u0456\u0442\u0438\
  \ \u043F\u043E\u0434\u0440\u043E\u0431\u0438\u0446\u0456. \u0414\u0430\u0442\u0438\
  \ \u0432 JavaScript \u043F\u0440\u0435\u0434\u0441\u0442\u0430\u0432\u043B\u0435\
  \u043D\u0456 \u044F\u043A \u043E\u0431\u2019\u0454\u043A\u0442\u0438 `Date`, \u0456\
  \ \u043A\u043E\u043B\u0438\u2026"
lastmod: '2024-04-05T22:51:02.914729-06:00'
model: gpt-4-1106-preview
summary: "\u041F\u043E\u0440\u0456\u0432\u043D\u044F\u043D\u043D\u044F \u0434\u0432\
  \u043E\u0445 \u0434\u0430\u0442 \u0443 JavaScript \u043C\u043E\u0436\u0435 \u0431\
  \u0443\u0442\u0438 \u043F\u0440\u043E\u0441\u0442\u0438\u043C, \u0430\u043B\u0435\
  \ \u0432\u0430\u0436\u043B\u0438\u0432\u043E \u0440\u043E\u0437\u0443\u043C\u0456\
  \u0442\u0438 \u043F\u043E\u0434\u0440\u043E\u0431\u0438\u0446\u0456."
title: "\u041F\u043E\u0440\u0456\u0432\u043D\u044F\u043D\u043D\u044F \u0434\u0432\u043E\
  \u0445 \u0434\u0430\u0442"
weight: 27
---

## Як це робити:
```javascript
// Створення двох дат
let date1 = new Date('2023-03-01');
let date2 = new Date('2023-04-01');

// Порівняння дат
if (date1 < date2) {
  console.log('Дата1 раніше, ніж Дата2.');
} else if (date1 > date2) {
  console.log('Дата1 пізніше, ніж Дата2.');
} else {
  console.log('Дата1 та Дата2 однакові.');
}

// Використання методу getTime() для точного порівняння
if (date1.getTime() === date2.getTime()) {
  console.log('Дата1 та Дата2 однакові до мілісекунди.');
}
```
Вивід:
```
Дата1 раніше, ніж Дата2.
```

## Поглиблений аналіз:
Порівняння двох дат у JavaScript може бути простим, але важливо розуміти подробиці. Дати в JavaScript представлені як об’єкти `Date`, і коли ми їх порівнюємо, насправді ми порівнюємо часові мітки (timestamp) — кількість мілісекунд з певної дати (1 січня 1970 року). Варто знати, що порівнювати об’єкти `Date` можна безпосередньо через оператори `>`, `<`, `===`, або використовувати метод `getTime()` для отримання числового представлення. Історично були альтернативні методи, як наприклад бібліотека `Moment.js`, але вони стають менш популярними через вдосконалення нативного API.

## Дивіться також:
- [MDN Web Docs: Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [ECMAScript 2022 Language Specification](https://tc39.es/ecma262/)
- [You Don't Need Moment.js](https://youmightnotneed.com/momentjs/)
- [Stack Overflow: How to compare two dates](https://stackoverflow.com/questions/492994/compare-two-dates-with-javascript)
