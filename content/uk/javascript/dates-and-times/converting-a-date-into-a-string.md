---
date: 2024-01-20 17:37:41.181701-07:00
description: "How To: (\u042F\u043A \u0437\u0440\u043E\u0431\u0438\u0442\u0438:) \u041F\
  \u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0434\u0430\u0442\
  \ \u0432 \u0440\u044F\u0434\u043A\u0438 \u2013 \u0441\u0442\u0430\u043D\u0434\u0430\
  \u0440\u0442\u043D\u0430 \u043F\u0440\u043E\u0446\u0435\u0434\u0443\u0440\u0430\
  \ \u0443 JavaScript \u0437 \u043C\u043E\u043C\u0435\u043D\u0442\u0443 \u0439\u043E\
  \u0433\u043E \u0441\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F. \u0427\u043E\
  \u043C\u0443? \u0422\u043E\u043C\u0443 \u0449\u043E \u0440\u043E\u0431\u043E\u0442\
  \u0430 \u0437 \u0434\u0430\u0442\u0430\u043C\u0438 \u0432\u0430\u0436\u043B\u0438\
  \u0432\u0430 \u0434\u043B\u044F\u2026"
lastmod: '2024-04-05T22:51:02.913523-06:00'
model: gpt-4-1106-preview
summary: "(\u042F\u043A \u0437\u0440\u043E\u0431\u0438\u0442\u0438:) \u041F\u0435\u0440\
  \u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0434\u0430\u0442 \u0432\
  \ \u0440\u044F\u0434\u043A\u0438 \u2013 \u0441\u0442\u0430\u043D\u0434\u0430\u0440\
  \u0442\u043D\u0430 \u043F\u0440\u043E\u0446\u0435\u0434\u0443\u0440\u0430 \u0443\
  \ JavaScript \u0437 \u043C\u043E\u043C\u0435\u043D\u0442\u0443 \u0439\u043E\u0433\
  \u043E \u0441\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F."
title: "\u041F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0434\
  \u0430\u0442\u0438 \u0432 \u0440\u044F\u0434\u043E\u043A"
weight: 28
---

## How To: (Як зробити:)
```javascript
// Створення нового об'єкту дати
const now = new Date();

// Конвертація в локальний рядковий формат
const localDateString = now.toLocaleDateString('uk-UA');
console.log(localDateString); // 'MM/dd/yyyy', де формат дати залежить від локації

// Конвертація в рядок за замовчуванням
const dateString = now.toString();
console.log(dateString); // "Wed Apr 05 2023 17:38:07 GMT+0300 (Eastern European Summer Time)"

// Конвертація в UTC рядок
const dateUtcString = now.toUTCString();
console.log(dateUtcString); // "Wed, 05 Apr 2023 14:38:07 GMT"

// Користувацьке форматування з toLocaleString
const customDateString = now.toLocaleString('uk-UA', { day: '2-digit', month: 'long', year: 'numeric' });
console.log(customDateString); // '05 квітня 2023 р.'
```

## Deep Dive (Поглиблене занурення):
Перетворення дат в рядки – стандартна процедура у JavaScript з моменту його створення. Чому? Тому що робота з датами важлива для логіки застосунків і користувальницького інтерфейсу.

Альтернативи? Можна використовувати бібліотеки як Moment.js або date-fns для більш зручної роботи з датами, але вони додають зайвий обсяг коду.

Деталі реалізації? Коли ви викликаєте `toString()`, дата конвертується в рядок у форматі ISO. Метод `toLocaleDateString()` залежить від локалі користувача. `toLocaleString()` може приймати параметри для кастомізації результату.

## See Also (Дивіться також):
- [Date.prototype.toString() - MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toString)
- [Date.prototype.toLocaleDateString() - MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleDateString)
- [Date.prototype.toUTCString() - MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toUTCString)
- [Date and time formatting with toLocaleString() - MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleString)
