---
date: 2024-01-20 17:32:52.515034-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : ."
lastmod: '2024-03-13T22:44:48.888706-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u041E\u0431\u0447\u0438\u0441\u043B\u0435\u043D\u043D\u044F \u0434\u0430\u0442\
  \u0438 \u0443 \u043C\u0430\u0439\u0431\u0443\u0442\u043D\u044C\u043E\u043C\u0443\
  \ \u0430\u0431\u043E \u043C\u0438\u043D\u0443\u043B\u043E\u043C\u0443"
weight: 26
---

## Як це зробити:
```TypeScript
// Додавання днів до поточної дати
function addDays(date: Date, days: number): Date {
  const result = new Date(date);
  result.setDate(result.getDate() + days);
  return result;
}

// Віднімання днів від поточної дати
function subtractDays(date: Date, days: number): Date {
  return addDays(date, -days);
}

// Приклад використання
const today = new Date();
console.log(today);               // Показує поточну дату
console.log(addDays(today, 10));  // Показує дату за 10 днів
console.log(subtractDays(today, 5)); // Показує дату 5 днів назад
```

## Пірнаємо глибше:
Використання класу `Date` у JavaScript і TypeScript для обрахунку дат — це базовий метод, який існує вже роками. Альтернативи, як `Moment.js` і `date-fns`, пропонують більше можливостей і зручностей, але з недавніх пір `Moment.js` більше не розвивається, тому `date-fns` чи навіть нативний `Intl` API можуть бути кращим вибором.

Обрахунок дат може включати часові пояси та літній час, що ускладнює завдання. Рекомендується використовувати бібліотеки, що можуть впоратися з цими питаннями.

Також, важливим є і коректна обробка високосних років і варіацій у кількості днів у місяцях. TypeScript не робить це автоматично, тому обережно використовуйте `setDate` і подібні методи, або застосовуйте зовнішні бібліотеки.

## Дивіться також:
- MDN Web Docs [Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [date-fns](https://date-fns.org/) — сучасна бібліотека для роботи з датами
- [Luxon](https://moment.github.io/luxon/) — міцна бібліотека для дат та часу, створена одними з колишніх розробників Moment.js
- [Day.js](https://day.js.org/) — легковага бібліотека для роботи з датами, що має схожий API з Moment.js
