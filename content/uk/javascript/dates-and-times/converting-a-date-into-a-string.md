---
title:                "Перетворення дати в рядок"
aliases:
- /uk/javascript/converting-a-date-into-a-string.md
date:                  2024-01-20T17:37:41.181701-07:00
model:                 gpt-4-1106-preview
simple_title:         "Перетворення дати в рядок"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/converting-a-date-into-a-string.md"
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
