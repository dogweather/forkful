---
title:                "Отримання поточної дати"
html_title:           "Bash: Отримання поточної дати"
simple_title:         "Отримання поточної дати"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Що та навіщо? 
Отримання поточної дати - це процес зчитування даних про хронологічний момент. Програмісти роблять це для відслідковування подій у часі, логування, генерації показників та ін..

## Як це зробити:
Ось простий приклад того, як отримати поточну дату в TypeScript:

```TypeScript
let currentDate = new Date();
console.log(currentDate);
```

Коли ви запустите цей код, система виведе поточну дату та час.

```TypeScript
// Output can be like this
2022-03-06T12:35:15.612Z
```

## Поглиблений розбір:
Раніше, щоб отримати дату ічас, програмісти використовували засоби операційної системи. Однак, з появою JavaScript та TypeScript, стало можливим отримувати поточну дату прямо в коді.

Є інші способи отримання поточної дати - використання бібліотек, таких як Moment.js, які надають більший контроль над форматом дати та часу.

Використання `new Date()` у типах Double і Integer дає миттєву конвертацію в мілісекунди з 1 січня 1970 року.

## Дивіться також:
1. [MDN Web Docs - Date](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/Date)
2. [Moment.js](https://momentjs.com/)