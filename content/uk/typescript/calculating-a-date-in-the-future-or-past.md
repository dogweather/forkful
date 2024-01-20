---
title:                "Розрахунок дати в майбутньому або минулому"
html_title:           "TypeScript: Розрахунок дати в майбутньому або минулому"
simple_title:         "Розрахунок дати в майбутньому або минулому"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

Title: Розрахунок дати у майбутньому чи минулому у TypeScript

## Що і чому?
Розрахунок дати у майбутньому чи минулому - це процес визначення дати, яка відбувається після або до певної дати. Програмісти роблять це, щоб керувати подіями, які мають відбутися наступнім часом або уже відбулися.

## Як це зробити:

Одним зі способів розрахунку дати у майбутньому або в минулому є використання об'єкту `Date` TypeScript та його методів.

```TypeScript
let now = new Date();

// Розрахувати дату за 5 днів в майбутньому
let futureDate = new Date();
futureDate.setDate(now.getDate() + 5);
console.log(`Дата у майбутньому: ${futureDate}`);

// Розрахувати дату за 5 днів у минулому
let pastDate = new Date();
pastDate.setDate(now.getDate() - 5);
console.log(`Дата у минулому: ${pastDate}`);
```

## Інтенсивне занурення

Розрахунок дати у майбутньому або минулому дуже важливий в деяких контекстах, наприклад, в обробці подій, управлінні розкладами і всім, що пов'язане з часом. TypeScript, являється сучасною версією JavaScript, надає вбудовані методи в об'єкті `Date` для виконання цих завдань.

Альтернативою може бути використання бібліотеки, такої як Moment.js, яка надає більше можливостей і гнучкості при роботі з датами і часом.

Реалізація розрахунку дати у майбутньому або минулому включає використання методу `setDate()`. Цей метод змінює день місяця вказаної дати і в той же час автоматично коригує місяць і рік, якщо це потрібно.

## Дивіться також

1. [Date - JavaScript | MDN](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/Date)
2. [Moment.js](https://momentjs.com/)