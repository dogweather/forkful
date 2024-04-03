---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:58:38.511565-07:00
description: "\u041A\u0430\u043A: ."
lastmod: '2024-03-13T22:44:45.779453-06:00'
model: gpt-4-0125-preview
summary: .
title: "\u041F\u043E\u043B\u0443\u0447\u0435\u043D\u0438\u0435 \u0442\u0435\u043A\u0443\
  \u0449\u0435\u0439 \u0434\u0430\u0442\u044B"
weight: 29
---

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
