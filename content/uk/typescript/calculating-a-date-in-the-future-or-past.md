---
title:                "Обчислення дати у майбутньому або минулому"
date:                  2024-01-20T17:32:52.515034-07:00
model:                 gpt-4-1106-preview
simple_title:         "Обчислення дати у майбутньому або минулому"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Що і Чому?
Обрахунок дат у майбутньому чи минулому — це визначення дати, яка відстоїть на певну кількість днів, місяців, або років від вказаного моменту. Програмісти використовують це для створення застосунків з планувальниками, нагадуваннями, та історичними записами.

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
