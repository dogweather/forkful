---
title:                "Отримання поточної дати"
aliases: - /uk/javascript/getting-the-current-date.md
date:                  2024-02-03T19:10:21.193946-07:00
model:                 gpt-4-0125-preview
simple_title:         "Отримання поточної дати"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?
Отримання поточної дати в JavaScript є фундаментальним завданням, що включає в себе отримання та можливо маніпулювання сьогоднішньою датою та часом. Програмісти виконують це, щоб відображати дати на вебсайтах, в додатках, для відстеження взаємодій користувачів або для обробки часово-чутливих даних.

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
