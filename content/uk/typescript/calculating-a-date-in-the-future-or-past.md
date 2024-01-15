---
title:                "Обчислення дати у майбутньому або минулому"
html_title:           "TypeScript: Обчислення дати у майбутньому або минулому"
simple_title:         "Обчислення дати у майбутньому або минулому"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

##Чому

Розрахування дати в майбутньому або минулому може бути корисно для планування подій та визначення термінів розрахунків.

##Як це зробити

```TypeScript
// Приклад коду для розрахунку дати в майбутньому
let date = new Date();
date.setFullYear(date.getFullYear() + 1);
console.log(date); // Виведе дату через рік

// Приклад коду для розрахунку дати в минулому
let date = new Date();
date.setFullYear(date.getFullYear() - 1);
console.log(date); // Виведе дату з минулим роком
```

Deep Dive: Розрахунок дати в TypeScript може бути здійснений за допомогою вбудованого типу даних Date. Методи `setFullYear()`, `setMonth()`, `setDate()` та інші, дозволяють змінити значення поточної дати в об'єкті Date. Також можна використовувати оператори математичних обчислень для розрахунку дати в майбутньому або минулому.

##Дивитися також

- [Документація TypeScript](https://www.typescriptlang.org/docs/)
- [Офіційний сайт мови TypeScript](https://www.typescriptlang.org/)
- [Розрахунок дати в JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)