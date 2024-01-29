---
title:                "Получение текущей даты"
date:                  2024-01-28T23:58:38.511565-07:00
model:                 gpt-4-0125-preview
simple_title:         "Получение текущей даты"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/javascript/getting-the-current-date.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Зачем?
Получение текущей даты в JavaScript означает получение даты и времени текущего дня. Программисты делают это для таких вещей, как временные метки, расписания и логика, основанная на времени.

## Как:
```javascript
const now = new Date();
console.log(now.toString());  // Пример вывода: Wed Apr 05 2023 20:46:28 GMT-0400 (Восточное летнее время)

console.log(now.toISOString());  // Пример вывода: 2023-04-05T20:46:28.000Z
```

## Подробнее
Давным-давно объект `Date` в JavaScript был создан для работы с датами и временем. Объект `Date` представляет собой один момент времени до миллисекунды.

**Альтернативы:**
- Библиотеки вроде Moment.js (хотя сейчас она считается устаревшей), date-fns или Luxon могут предложить больше функций.
- С Node.js можно использовать встроенные модули для времени, но в большинстве случаев родной объект `Date` работает вполне удовлетворительно.

**Детали реализации:**
- `Date` может превратиться в строку или в определённый формат с помощью методов вроде `.toString(), .toISOString()`.
- Проблемы, связанные с часовыми поясами, часто являются проблемными местами. Обратите внимание, `.toISOString()` возвращает время в UTC.
- JavaScript считает время как миллисекунды с момента Unix Epoch (1 января 1970 года, 00:00:00 UTC). Получить это можно с помощью `Date.now()`.

## Смотри также
- [MDN Web Docs о Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Вам не нужен Moment.js](https://you-dont-need.github.io/You-Dont-Need-Momentjs/)
- [Документация Luxon](https://moment.github.io/luxon/)
