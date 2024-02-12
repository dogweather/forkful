---
title:                "Обчислення дати у майбутньому або минулому"
aliases: - /uk/javascript/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:31:25.555221-07:00
model:                 gpt-4-1106-preview
simple_title:         "Обчислення дати у майбутньому або минулому"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Що і Навіщо?

Обчислення дат у майбутньому чи минулому - це спосіб визначення дат до або після заданої точки в часі. Програмісти використовують це для розкладів, таймерів, термінів дії чи будь-чого, де важливий час.

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
