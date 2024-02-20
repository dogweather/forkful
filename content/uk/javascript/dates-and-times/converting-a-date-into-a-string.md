---
date: 2024-01-20 17:37:41.181701-07:00
description: "\u041F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F\
  \ \u0434\u0430\u0442\u0438 \u0443 \u0440\u044F\u0434\u043E\u043A \u2014 \u043F\u0440\
  \u043E\u0446\u0435\u0441, \u0434\u0435 \u043E\u0431'\u0454\u043A\u0442 \u0434\u0430\
  \u0442\u0438 JavaScript \u0441\u0442\u0430\u0454 \u0447\u0438\u0442\u0430\u0431\u0435\
  \u043B\u044C\u043D\u0438\u043C \u0442\u0435\u043A\u0441\u0442\u043E\u043C. \u041F\
  \u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0432\u0438\u043A\u043E\
  \u043D\u0443\u044E\u0442\u044C \u0446\u0435 \u0434\u043B\u044F \u0432\u0456\u0434\
  \u043E\u0431\u0440\u0430\u0436\u0435\u043D\u043D\u044F \u0434\u0430\u0442 \u0443\
  \ \u0437\u0440\u043E\u0437\u0443\u043C\u0456\u043B\u043E\u043C\u0443\u2026"
lastmod: 2024-02-19 22:05:09.095296
model: gpt-4-1106-preview
summary: "\u041F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F\
  \ \u0434\u0430\u0442\u0438 \u0443 \u0440\u044F\u0434\u043E\u043A \u2014 \u043F\u0440\
  \u043E\u0446\u0435\u0441, \u0434\u0435 \u043E\u0431'\u0454\u043A\u0442 \u0434\u0430\
  \u0442\u0438 JavaScript \u0441\u0442\u0430\u0454 \u0447\u0438\u0442\u0430\u0431\u0435\
  \u043B\u044C\u043D\u0438\u043C \u0442\u0435\u043A\u0441\u0442\u043E\u043C. \u041F\
  \u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0432\u0438\u043A\u043E\
  \u043D\u0443\u044E\u0442\u044C \u0446\u0435 \u0434\u043B\u044F \u0432\u0456\u0434\
  \u043E\u0431\u0440\u0430\u0436\u0435\u043D\u043D\u044F \u0434\u0430\u0442 \u0443\
  \ \u0437\u0440\u043E\u0437\u0443\u043C\u0456\u043B\u043E\u043C\u0443\u2026"
title: "\u041F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0434\
  \u0430\u0442\u0438 \u0432 \u0440\u044F\u0434\u043E\u043A"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)
Перетворення дати у рядок — процес, де об'єкт дати JavaScript стає читабельним текстом. Програмісти виконують це для відображення дат у зрозумілому форматі або для передачі даних між сервером і клієнтом.

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
