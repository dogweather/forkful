---
title:                "Розбір дати з рядка"
aliases:
- /uk/javascript/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:14:43.217553-07:00
model:                 gpt-4-0125-preview
simple_title:         "Розбір дати з рядка"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?
Аналіз дати з рядка дозволяє програмістам перетворювати текстові представлення дат у об'єкти `Date` JavaScript, сприяючи маніпуляціям з датами, порівнянням та операціям форматування. Цей процес є важливим для обробки введення користувача, обробки даних з баз даних або роботи з API, які передають дати у форматі рядків.

## Як:
JavaScript нативно пропонує метод `Date.parse()` та конструктор `Date` для аналізу рядків з датами. Однак, ці підходи мають обмеження та невідповідності між різними браузерами, особливо з нестандартними форматами дат. Щоб вирішити ці проблеми, такі сторонні бібліотеки як `Moment.js` та `date-fns` є популярними за їх надійність та легкість у використанні.

### Використання нативного JavaScript:
```javascript
const dateString = "2023-04-30T14:55:00";
const dateObj = new Date(dateString);

console.log(dateObj);  // Вивід: Неділя, 30 квітня 2023 14:55:00 GMT+0000 (загальний світовий час)
```

### Використання Moment.js:
Спочатку встановіть Moment.js через npm або додайте його до свого проекту. Потім:
```javascript
const moment = require('moment');

const dateString = "2023-04-30T14:55:00";
const dateObj = moment(dateString);

console.log(dateObj.toString());  // Вивід: Неділя, 30 квітня 2023 14:55:00 GMT+0000
```

### Використання date-fns:
Після додавання `date-fns` до вашого проекту, розберіть рядок дати так:
```javascript
const { parseISO } = require('date-fns');

const dateString = "2023-04-30T14:55:00";
const dateObj = parseISO(dateString);

console.log(dateObj);  // Вивід: 2023-04-30T14:55:00.000Z
```

Як `Moment.js`, так і `date-fns` надають більш всебічні можливості аналізу, включаючи обробку різноманітних форматів та локалей, що робить їх переважними для складних застосунків.
