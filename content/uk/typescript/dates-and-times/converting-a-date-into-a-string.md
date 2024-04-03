---
date: 2024-01-20 17:37:52.640242-07:00
description: "\u041F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F\
  \ \u0434\u0430\u0442\u0438 \u0432 \u0440\u044F\u0434\u043E\u043A \u2014 \u0446\u0435\
  \ \u0441\u043F\u043E\u0441\u0456\u0431 \u043F\u0440\u0435\u0434\u0441\u0442\u0430\
  \u0432\u043B\u0435\u043D\u043D\u044F \u0447\u0430\u0441\u043E\u0432\u0438\u0445\
  \ \u0437\u043D\u0430\u0447\u0435\u043D\u044C \u0432 \u0437\u0440\u0443\u0447\u043D\
  \u043E\u043C\u0443 \u0434\u043B\u044F \u0447\u0438\u0442\u0430\u043D\u043D\u044F\
  \ \u0444\u043E\u0440\u043C\u0430\u0442\u0456. \u041F\u0440\u043E\u0433\u0440\u0430\
  \u043C\u0456\u0441\u0442\u0438 \u0446\u0435 \u0440\u043E\u0431\u043B\u044F\u0442\
  \u044C \u0434\u043B\u044F \u0432\u0456\u0437\u0443\u0430\u043B\u0456\u0437\u0430\
  \u0446\u0456\u0457, \u043B\u043E\u0433\u0443\u0432\u0430\u043D\u043D\u044F, \u0447\
  \u0438\u2026"
lastmod: '2024-03-13T22:44:48.886013-06:00'
model: gpt-4-1106-preview
summary: "\u041F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F\
  \ \u0434\u0430\u0442\u0438 \u0432 \u0440\u044F\u0434\u043E\u043A \u2014 \u0446\u0435\
  \ \u0441\u043F\u043E\u0441\u0456\u0431 \u043F\u0440\u0435\u0434\u0441\u0442\u0430\
  \u0432\u043B\u0435\u043D\u043D\u044F \u0447\u0430\u0441\u043E\u0432\u0438\u0445\
  \ \u0437\u043D\u0430\u0447\u0435\u043D\u044C \u0432 \u0437\u0440\u0443\u0447\u043D\
  \u043E\u043C\u0443 \u0434\u043B\u044F \u0447\u0438\u0442\u0430\u043D\u043D\u044F\
  \ \u0444\u043E\u0440\u043C\u0430\u0442\u0456."
title: "\u041F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0434\
  \u0430\u0442\u0438 \u0432 \u0440\u044F\u0434\u043E\u043A"
weight: 28
---

## Як це зробити:
```TypeScript
const currentDate: Date = new Date();

// Перетворення в локально зрозумілий рядок
const dateString1: string = currentDate.toLocaleDateString("uk-UA");
console.log(dateString1); // 'dd.mm.yyyy' (формат може варіюватися в залежності від браузера і ОС)

// Перетворення в стандартний ISO формат
const dateString2: string = currentDate.toISOString();
console.log(dateString2); // 'yyyy-mm-ddTHH:MM:SS.sssZ'

// Форматування з використанням Intl.DateTimeFormat
const formatOptions: Intl.DateTimeFormatOptions = { weekday: 'long', year: 'numeric', month: 'long', day: 'numeric' };
const localDateFormatter = new Intl.DateTimeFormat('uk-UA', formatOptions);
const dateString3: string = localDateFormatter.format(currentDate);
console.log(dateString3); // 'четвер, 8 березня 2023 р.'
```

## Поглиблення:
Перетворення дат в рядки не нове - це робили ще до появи JavaScript. Сучасні можливості TypeScript базуються на засобах JavaScript. Розглянемо декілька альтернатив:
- `Date.prototype.toString()`: просте перетворення без можливості вказати формат.
- Бібліотеки, як `moment.js` чи `date-fns`, пропонують широкий спектр функцій для роботи з датами та форматування.
- `Intl.DateTimeFormat`: частина міжнародної стандартної бібліотеки ECMAScript для форматування з урахуванням локалі. Вибір формату залежить від потреби.

Конвертація дат з TypeScript може використовувати будь-які вбудовані або зовнішні методи, що доступні у JavaScript. Важливо враховувати часові пояси й локалізацію при представленні дат користувачам.

## Дивіться також:
- MDN Web Docs про `Date`: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date
- Документація `Intl.DateTimeFormat`: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DateTimeFormat
- `moment.js`: https://momentjs.com/
- `date-fns`: https://date-fns.org/

Ці ресурси допоможуть збагнути різноманітність способів форматування та перетворення дат.
