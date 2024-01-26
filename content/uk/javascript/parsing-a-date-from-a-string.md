---
title:                "Аналіз дати з рядка"
date:                  2024-01-20T15:37:36.519985-07:00
html_title:           "Arduino: Аналіз дати з рядка"
simple_title:         "Аналіз дати з рядка"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? / Що таке та навіщо?
Розбір дати зі строки – це перетворення текстового представлення дати у формат, який програма може легко обробити. Робимо це для того, щоб працювати з датами: порівнювати, додавати час, відстежувати події.

## How to: / Як це зробити:
```javascript
// Приклад 1: Стандартний ISO формат
const isoDateStr = '2023-04-05T14:23:00Z';
const date1 = new Date(isoDateStr);
console.log(date1); // Wed Apr 05 2023 16:23:00 GMT+0200 (Eastern European Summer Time)

// Приклад 2: Нестандартний формат
const customDateStr = '05.04.2023 14:23:00';
const date2 = new Date(customDateStr);
console.log(date2); // Invalid Date

// Виправлення некоректного формату з використанням бібліотеки (наприклад, date-fns)
const { parse } = require('date-fns');
const date3 = parse(customDateStr, 'dd.MM.yyyy HH:mm:ss', new Date());
console.log(date3); // Wed Apr 05 2023 14:23:00 GMT+0200 (Eastern European Summer Time)
```

## Deep Dive / Поглиблений розгляд:
Розбір дати зі строки був складним завжди. Раніше, без стандартизації, розробники вимушені були писати велику кількість коду для простих завдань. З часом з'явились стандарти, такі як ISO 8601 для часу та дати, що полегшило життя.

Браузери різні, і не всі правильно розбирають нестандартні строки. Тому для надійності варто використовувати бібліотеки, як-от date-fns або moment.js (хоча останній уже не рекомендують для нових проектів).

Ключова річ у розборі дати – знати формат вхідної строки. Коли формат зрозумілий, ви можете користуватися Date constructor або бібліотеками для точного розбору та маніпуляцій зі зрозумілою датою.

## See Also / Додаткові ресурси:
- MDN Web Docs по Date: https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/Date
- Про ISO 8601 на Вікіпедії: https://uk.wikipedia.org/wiki/ISO_8601
- Документація date-fns: https://date-fns.org/
- Про moment.js (та рекомендації по переходу на інші бібліотеки): https://momentjs.com/docs/#/-project-status/
