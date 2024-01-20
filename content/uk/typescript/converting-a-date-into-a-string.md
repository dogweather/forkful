---
title:                "Перетворення дати в рядок"
html_title:           "Lua: Перетворення дати в рядок"
simple_title:         "Перетворення дати в рядок"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Що і чому?
Перетворення дати в рядок — це процес «перекладу» об'єктів дати на рядкове представлення. Програмісти роблять це для зручного показу дат користувачам або для зберігання дати в текстовому форматі.

## Як це зробити:
Нижче наведено приклади коду та приклади виводу:
```TypeScript
let currentDate = new Date();
let dateStr = currentDate.toISOString();
console.log(dateStr);
// Виведення: 2021-08-06T14:12:27.612Z
```
В цьому коді є об'єкт `currentDate` типу `Date`, який перетворюється на рядок за допомогою методу `.toISOString()`.
 

## Глибоке занурення
**Історичний контекст**: Метод `toISOString()` в JavaScript/TypeScript був стандартизований ще в ES5 і є одним із найпопулярніших для перетворення дат в рядок.

**Альтернативи**: Javascript/TypeScript пропонує різні методи перетворення дати в рядок. Окрім `.toISOString()`, є методи як `.toString()`, `.toLocaleString()`, `.toDateString()`, `.toTimeString()` і інші.

**Деталі реалізації**: Метод `.toISOString()` повертає дату в форматі ISO, тобто `YYYY-MM-DDTHH:mm:ss.sssZ`, де `Z` позначає UTC-час.

## Дивіться також
1. [MDN Web Docs: Date.prototype.toISOString()](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/Date/toISOString) - довідка MDN щодо `toISOString()`.
2. [Сайт TypeScript](https://www.typescriptlang.org/docs/) - Документація TypeScript.
3. [Кращі практики з JavaScript/TypeScript](https://github.com/airbnb/javascript) - База знань від Airbnb з наголосом на кращі практики.