---
title:                "Перетворення дати в рядок"
aliases:
- uk/typescript/converting-a-date-into-a-string.md
date:                  2024-01-20T17:37:52.640242-07:00
model:                 gpt-4-1106-preview
simple_title:         "Перетворення дати в рядок"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Що і Чому?
Перетворення дати в рядок — це спосіб представлення часових значень в зручному для читання форматі. Програмісти це роблять для візуалізації, логування, чи збереження дат в базах даних.

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
