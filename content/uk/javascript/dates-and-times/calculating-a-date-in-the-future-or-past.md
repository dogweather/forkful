---
date: 2024-01-20 17:31:25.555221-07:00
description: "\u042F\u043A \u0440\u043E\u0431\u0438\u0442\u0438: \u041A\u043B\u0430\
  \u0441 `Date` \u0432 JavaScript \u0456\u0441\u043D\u0443\u0454 \u0437 \u043F\u043E\
  \u0447\u0430\u0442\u043A\u043E\u0432\u0438\u0445 \u0432\u0435\u0440\u0441\u0456\u0439\
  \ \u043C\u043E\u0432\u0438 \u0456 \u0454 \u043E\u0441\u043D\u043E\u0432\u043D\u0438\
  \u043C \u0437\u0430\u0441\u043E\u0431\u043E\u043C \u0434\u043B\u044F \u0440\u043E\
  \u0431\u043E\u0442\u0438 \u0437 \u0434\u0430\u0442\u0430\u043C\u0438 \u0456 \u0447\
  \u0430\u0441\u043E\u043C. \u0410\u043B\u044C\u0442\u0435\u0440\u043D\u0430\u0442\
  \u0438\u0432\u0438, \u044F\u043A `moment.js`, \u043A\u043E\u043B\u0438\u0441\u044C\
  \u2026"
lastmod: '2024-04-05T22:51:02.915940-06:00'
model: gpt-4-1106-preview
summary: "\u041A\u043B\u0430\u0441 `Date` \u0432 JavaScript \u0456\u0441\u043D\u0443\
  \u0454 \u0437 \u043F\u043E\u0447\u0430\u0442\u043A\u043E\u0432\u0438\u0445 \u0432\
  \u0435\u0440\u0441\u0456\u0439 \u043C\u043E\u0432\u0438 \u0456 \u0454 \u043E\u0441\
  \u043D\u043E\u0432\u043D\u0438\u043C \u0437\u0430\u0441\u043E\u0431\u043E\u043C\
  \ \u0434\u043B\u044F \u0440\u043E\u0431\u043E\u0442\u0438 \u0437 \u0434\u0430\u0442\
  \u0430\u043C\u0438 \u0456 \u0447\u0430\u0441\u043E\u043C."
title: "\u041E\u0431\u0447\u0438\u0441\u043B\u0435\u043D\u043D\u044F \u0434\u0430\u0442\
  \u0438 \u0443 \u043C\u0430\u0439\u0431\u0443\u0442\u043D\u044C\u043E\u043C\u0443\
  \ \u0430\u0431\u043E \u043C\u0438\u043D\u0443\u043B\u043E\u043C\u0443"
weight: 26
---

## Як робити:
```Javascript
const today = new Date();
const daysToAdd = 5;

// Додавання днів
const futureDate = new Date(today);
futureDate.setDate(futureDate.getDate() + daysToAdd);

console.log(`Сьогодні: ${today.toDateString()}`);
console.log(`Дата в майбутньому: ${futureDate.toDateString()}`);

// Віднімання днів
const daysToSubtract = 3;
const pastDate = new Date(today);
pastDate.setDate(pastDate.getDate() - daysToSubtract);

console.log(`Дата в минулому: ${pastDate.toDateString()}`);
```

Сампл виводу:

```
Сьогодні: Wed Apr 05 2023
Дата в майбутньому: Mon Apr 10 2023
Дата в минулому: Sun Apr 02 2023
```

## Детальніше:
Клас `Date` в JavaScript існує з початкових версій мови і є основним засобом для роботи з датами і часом. Альтернативи, як `moment.js`, колись були популярніши за умовчання в JavaScript, але тепер, з поліпшенням базової мови, часто достатньо стандартного `Date`. Деталі, як часові зони та перехід на літній/зимовий час, можуть ускладнити роботу з датами. Так звані ISO строки (`toISOString()`) і `Date.UTC()` можуть бути корисними для работы в универсальному часовому форматі (UTC).

## Також дивіться:
- [MDN Web Docs: Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date) - Офіційна документація класу `Date`.
- [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601) - Стандарт оформлення дат та часу, корисний для міжнародних застосунків.
- [You Don't Need Moment.js](https://you-dont-need.github.io/You-Dont-Need-Momentjs/) - Поради як обходитися без додаткових бібліотек для роботи з датами.
