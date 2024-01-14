---
title:                "TypeScript: Перетворення дати в рядок"
simple_title:         "Перетворення дати в рядок"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Чому

Конвертування дати в рядок є важливою задачею для багатьох програмістів. Це дозволяє приводити дані до читабельного формату, що забезпечує більш зручне використання та обробку дат у програмах.

## Як

Для конвертування дати в рядок використовуються методи, доступні в класі `Date`. Наприклад, для отримання рядка, що містить дату та час у зазначеному форматі, можна використовувати метод `toLocaleString()`.

```TypeScript
const date = new Date(); // створюємо об'єкт дати
const stringDate = date.toLocaleString(); // отримуємо рядок у форматі "mm/dd/yyyy, hh:mm:ss am/pm"
console.log(stringDate); // виводимо результат у консоль
```

Ви також можете використовувати методи `getFullYear()`, `getMonth()`, `getDate()` та інші, щоб отримати окремі значення року, місяця, дня тощо. Детальніше про ці методи можна дізнатися у [документації](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/Date).

Зверніть увагу, що методи `toLocaleString()`, `getFullYear()` та інші можуть приймати аргументи для встановлення бажаного формату та мови. Наприклад, `date.toLocaleString("uk-UA", {weekday: "long"})` поверне день тижня у форматі "п'ятниця".

## Deep Dive

Конвертування дати в рядок може бути складнішою задачею за перший погляд. Деякі складнощі можуть виникнути при використанні різних мов та різних форматів дат. Також важливо пам'ятати про різницю між локальними та глобальними часовими зонами.

Щоб уникнути проблем з конвертацією дат, бажано використовувати сторонні бібліотеки, які вже мають вбудовані функції конвертування дат у рядки для різних мов та форматів. Наприклад, такою бібліотекою є [moment.js](https://momentjs.com/). Вона також має багато корисних функцій для роботи з датами.

## Дивіться також

- [Документація з роботи з датами в TypeScript](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-3-7.html#better-support-for-readonly-arrays-and-tuples)
- [Стаття про роботу з датами в JavaScript](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/Date/Date)
- [Бібліотека moment.js](https://momentjs.com/)