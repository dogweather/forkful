---
title:                "Розрахунок дати в майбутньому або минулому"
html_title:           "Javascript: Розрахунок дати в майбутньому або минулому"
simple_title:         "Розрахунок дати в майбутньому або минулому"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Що та чому?
Розрахунок дати в майбутньому або минулому - це процес визначення точної дати, яка буде відстоювати певну кількість часу від поточної дати. Програмісти використовують цей підхід для різних цілей, таких як створення розкладів, планування подій або реалізації функцій, пов'язаних з визначенням строків.

# Як це зробити:
```Javascript
// Розрахунок дати в майбутньому (додавання днів)
let currentDate = new Date(); // поточна дата
let numberOfDaysToAdd = 7; // кількість днів, які будемо додавати
let futureDate = new Date(currentDate.setDate(currentDate.getDate() + numberOfDaysToAdd)); // дата, відстояна на 7 днів від поточної

console.log(futureDate.toDateString()); // Вивід дати у зрозумілому форматі

// Розрахунок дати в минулому (віднімання років)
let currentDate = new Date();
let numberOfYearsToSubtract = 5;
let pastDate = new Date(currentDate.getFullYear() - numberOfYearsToSubtract, currentDate.getMonth(), currentDate.getDate());

console.log(pastDate.toDateString());
```

# Вивчення глибше:
- Історичний контекст: методи розрахунку дати існують з часів створення комп'ютерів і були широко використані для обчислення термінів в середовищі бізнесу та наукових досліджень.
- Альтернативи: для розрахунку дати в майбутньому або минулому можна використовувати бібліотеки, такі як Moment.js або Date-fns, які містять готові функції для цього.
- Деталі реалізації: методи, які використовуються для розрахунку дати в Javascript - це вбудовані методи конструктора Date, такі як `setDate()` та `setFullYear()`.

# Дивіться також:
- [MDN Документація про розрахунок дати в Javascript](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js бібліотека для розрахунку дати в майбутньому або минулому](https://momentjs.com/docs/)
- [Date-fns бібліотека для розрахунку дати в майбутньому або минулому](https://date-fns.org/docs/Getting-Started)