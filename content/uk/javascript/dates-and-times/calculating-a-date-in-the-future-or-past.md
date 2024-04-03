---
date: 2024-01-20 17:31:25.555221-07:00
description: "\u041E\u0431\u0447\u0438\u0441\u043B\u0435\u043D\u043D\u044F \u0434\u0430\
  \u0442 \u0443 \u043C\u0430\u0439\u0431\u0443\u0442\u043D\u044C\u043E\u043C\u0443\
  \ \u0447\u0438 \u043C\u0438\u043D\u0443\u043B\u043E\u043C\u0443 - \u0446\u0435 \u0441\
  \u043F\u043E\u0441\u0456\u0431 \u0432\u0438\u0437\u043D\u0430\u0447\u0435\u043D\u043D\
  \u044F \u0434\u0430\u0442 \u0434\u043E \u0430\u0431\u043E \u043F\u0456\u0441\u043B\
  \u044F \u0437\u0430\u0434\u0430\u043D\u043E\u0457 \u0442\u043E\u0447\u043A\u0438\
  \ \u0432 \u0447\u0430\u0441\u0456. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\
  \u0441\u0442\u0438 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\
  \u044E\u0442\u044C \u0446\u0435 \u0434\u043B\u044F \u0440\u043E\u0437\u043A\u043B\
  \u0430\u0434\u0456\u0432,\u2026"
lastmod: '2024-03-13T22:44:50.017173-06:00'
model: gpt-4-1106-preview
summary: "\u041E\u0431\u0447\u0438\u0441\u043B\u0435\u043D\u043D\u044F \u0434\u0430\
  \u0442 \u0443 \u043C\u0430\u0439\u0431\u0443\u0442\u043D\u044C\u043E\u043C\u0443\
  \ \u0447\u0438 \u043C\u0438\u043D\u0443\u043B\u043E\u043C\u0443 - \u0446\u0435 \u0441\
  \u043F\u043E\u0441\u0456\u0431 \u0432\u0438\u0437\u043D\u0430\u0447\u0435\u043D\u043D\
  \u044F \u0434\u0430\u0442 \u0434\u043E \u0430\u0431\u043E \u043F\u0456\u0441\u043B\
  \u044F \u0437\u0430\u0434\u0430\u043D\u043E\u0457 \u0442\u043E\u0447\u043A\u0438\
  \ \u0432 \u0447\u0430\u0441\u0456."
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
