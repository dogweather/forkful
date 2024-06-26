---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:21.193946-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0423 \u0447\u0438\u0441\u0442\u043E\u043C\u0443 JavaScript, \u043E\u0431'\u0454\
  \u043A\u0442 `Date` \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\
  \u0443\u0454\u0442\u044C\u0441\u044F \u0434\u043B\u044F \u0440\u043E\u0431\u043E\
  \u0442\u0438 \u0437 \u0434\u0430\u0442\u0430\u043C\u0438 \u0442\u0430 \u0447\u0430\
  \u0441\u043E\u043C. \u041E\u0441\u044C \u044F\u043A \u0432\u0438 \u043C\u043E\u0436\
  \u0435\u0442\u0435 \u043E\u0442\u0440\u0438\u043C\u0430\u0442\u0438 \u043F\u043E\
  \u0442\u043E\u0447\u043D\u0443 \u0434\u0430\u0442\u0443 \u0442\u0430 \u0447\u0430\
  \u0441."
lastmod: '2024-03-13T22:44:50.012026-06:00'
model: gpt-4-0125-preview
summary: "\u0423 \u0447\u0438\u0441\u0442\u043E\u043C\u0443 JavaScript, \u043E\u0431\
  '\u0454\u043A\u0442 `Date` \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\
  \u0432\u0443\u0454\u0442\u044C\u0441\u044F \u0434\u043B\u044F \u0440\u043E\u0431\
  \u043E\u0442\u0438 \u0437 \u0434\u0430\u0442\u0430\u043C\u0438 \u0442\u0430 \u0447\
  \u0430\u0441\u043E\u043C."
title: "\u041E\u0442\u0440\u0438\u043C\u0430\u043D\u043D\u044F \u043F\u043E\u0442\u043E\
  \u0447\u043D\u043E\u0457 \u0434\u0430\u0442\u0438"
weight: 29
---

## Як це зробити:
У чистому JavaScript, об'єкт `Date` використовується для роботи з датами та часом. Ось як ви можете отримати поточну дату та час:

```javascript
const currentDate = new Date();
console.log(currentDate); // Приклад виводу: Птн Кві 14 2023 12:34:56 GMT+0100 (Британський літній час)
```

Щоб показати лише дату в більш дружньому форматі, можна використовувати методи на кшталт `toLocaleDateString()`:

```javascript
console.log(currentDate.toLocaleDateString()); // Приклад виводу: 14.04.2023
```

Для більшого контролю над форматом, дуже популярними є сторонні бібліотеки на кшталт *Moment.js* або *date-fns*, хоча варто знати, що Moment.js наразі вважається застарілим проектом, що підтримується.

Використання *Moment.js*:

```javascript
const moment = require('moment'); // припускаючи, що використовується Node.js або модульний пакувальник
const formattedDate = moment().format('YYYY-MM-DD');
console.log(formattedDate); // Приклад виводу: 2023-04-14
```

З *date-fns*, яка акцентує увагу на модульності, дозволяючи імпортувати лише те, що вам потрібно:

```javascript
const { format } = require('date-fns');
const formattedDate = format(new Date(), 'yyyy-MM-dd');
console.log(formattedDate); // Приклад виводу: 2023-04-14
```

Кожен підхід пропонує різні рівні зручності та гнучкості при роботі з датами у JavaScript, від вбудованого об’єкта `Date` до більш складних можливостей форматування та маніпулювання, що доступні через бібліотеки.
