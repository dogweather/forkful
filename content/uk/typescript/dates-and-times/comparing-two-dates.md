---
date: 2024-01-20 17:34:13.096247-07:00
description: "\u042F\u043A \u0437\u0440\u043E\u0431\u0438\u0442\u0438: \u0412 JavaScript\
  \ \u0456 TypeScript \u0434\u0430\u0442\u0438 \u043F\u043E\u0440\u0456\u0432\u043D\
  \u044E\u044E\u0442\u044C\u0441\u044F \u044F\u043A \u0447\u0438\u0441\u043B\u0430\
  , \u0442\u0430\u043A \u044F\u043A \u0432\u043E\u043D\u0438 \u043F\u0440\u0435\u0434\
  \u0441\u0442\u0430\u0432\u043B\u0435\u043D\u0456 \u043A\u0456\u043B\u044C\u043A\u0456\
  \u0441\u0442\u044E \u043C\u0456\u043B\u0456\u0441\u0435\u043A\u0443\u043D\u0434\
  \ \u0437 1970 \u0440\u043E\u043A\u0443 (Unix Time Stamp). \u0427\u0430\u0441\u043E\
  \u0432\u0456 \u0437\u043E\u043D\u0438\u2026"
lastmod: '2024-04-05T21:53:49.112996-06:00'
model: gpt-4-1106-preview
summary: "\u0412 JavaScript \u0456 TypeScript \u0434\u0430\u0442\u0438 \u043F\u043E\
  \u0440\u0456\u0432\u043D\u044E\u044E\u0442\u044C\u0441\u044F \u044F\u043A \u0447\
  \u0438\u0441\u043B\u0430, \u0442\u0430\u043A \u044F\u043A \u0432\u043E\u043D\u0438\
  \ \u043F\u0440\u0435\u0434\u0441\u0442\u0430\u0432\u043B\u0435\u043D\u0456 \u043A\
  \u0456\u043B\u044C\u043A\u0456\u0441\u0442\u044E \u043C\u0456\u043B\u0456\u0441\u0435\
  \u043A\u0443\u043D\u0434 \u0437 1970 \u0440\u043E\u043A\u0443 (Unix Time Stamp)."
title: "\u041F\u043E\u0440\u0456\u0432\u043D\u044F\u043D\u043D\u044F \u0434\u0432\u043E\
  \u0445 \u0434\u0430\u0442"
weight: 27
---

## Як зробити:
```TypeScript
// Створюємо дві дати
const date1 = new Date('2023-04-01T00:00:00');
const date2 = new Date('2023-04-02T00:00:00');

// Порівнюємо дати
if (date1 < date2) {
  console.log('date1 є раніше date2');
} else if (date1 > date2) {
  console.log('date1 є пізніше date2');
} else {
  console.log('date1 та date2 є однакові');
}

// Вивід: 'date1 є раніше date2'
```

## Поглиблений огляд
В JavaScript і TypeScript дати порівнюються як числа, так як вони представлені кількістю мілісекунд з 1970 року (Unix Time Stamp). Часові зони можуть впливати на результати порівняння, тому розробники мають бути уважні при роботі з ними. Альтернативами є бібліотеки, наприклад Moment.js або date-fns, які можуть спрощувати роботу з датами і часовими зонами.

## Дивіться також:
- MDN Web Docs про роботу з датами у JavaScript: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date
- Date-fns бібліотека для роботи з датами: https://date-fns.org/
- Moment.js бібліотека та документація: https://momentjs.com/docs/#/parsing/unix-timestamp/
