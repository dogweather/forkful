---
title:    "TypeScript: Обчислення дати в майбутньому або минулому"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Чому

Розрахунок дати у майбутнє або в минуле може бути корисним для багатьох причин, наприклад, планування подій, встановлення термінів або пошуку історичних даних. Використання TypeScript для цього завдання може робити процес більш простим та ефективним.

## Як

Більшість програмістів вже знають про вбудовану JavaScript функцію `Date()` для отримання поточної дати. У TypeScript також є така сама функція, але для розрахунку дати в майбутньому або в минулому потрібно використовувати інші методи.

```TypeScript
// Розрахунок дати у майбутнє
let currDate = new Date();
let futureDate = new Date(currDate.getFullYear(), currDate.getMonth() + 1, currDate.getDate()); 
// Дата через 1 місяць
console.log(futureDate.toDateString()); // Sun Jun 20 2021

// Розрахунок дати в минулому
let pastDate = new Date(currDate.getFullYear(), currDate.getMonth() - 1, currDate.getDate());
// Дата минулого місяця
console.log(pastDate.toDateString()); // Fri Apr 20 2021
```

Розглянутий приклад дозволяє отримувати потрібні дати, визначаючи кількість місяців, днів або років (або їх комбінації) до або після поточної дати.

## Глибоке занурення

При розрахунку дати у майбутнє або в минулому, важливо враховувати різницю у місяцях та роках - особливо при переході через межу року, наприклад, з грудня на січень. Також слід пам'ятати про властивості `year`, `month` та `date` об'єкту `Date`, які вказують на поточну дату, а не на майбутню або минулу. Використовуючи ці знання, можна більш точно розраховувати дати.

## Дивіться також

- [Документація TypeScript про об'єкт Date](https://www.typescriptlang.org/docs/handbook/standard-library.html#date)
- [Stack Overflow: Код для розрахунку дати в майбутньому або в минулому](https://stackoverflow.com/questions/49786107/typescript-add-months-to-date)
- [Цікаві факти про обчислення дат в програмуванні](https://dmitripavlutin.com/dates-add-remove-js/)