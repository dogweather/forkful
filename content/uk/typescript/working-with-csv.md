---
title:                "Робота з CSV файлами"
html_title:           "Arduino: Робота з CSV файлами"
simple_title:         "Робота з CSV файлами"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/working-with-csv.md"
---

{{< edit_this_page >}}

## Що таке & Чому?
Робота з CSV (Comma-Separated Values) полягає в організації та обробці даних, які зберігаються у текстовому форматі з комами як роздільниками. Програмісти використовують CSV через його простоту та широку підтримку різними інструментами й платформами.

## Як це зробити:
```TypeScript
import * as fs from 'fs';
import * as csv from 'fast-csv';

// Читання CSV файлу
fs.createReadStream('data.csv')
  .pipe(csv.parse({ headers: true }))
  .on('data', (row) => console.log(row))
  .on('end', () => console.log('CSV файл успішно оброблений.'));

// Запис у CSV файл
const data = [
  { name: 'Іван', age: 34 },
  { name: 'Олена', age: 28 }
];

fs.createWriteStream('output.csv')
  .pipe(csv.format({ headers: true }))
  .write(data)
  .end();
```
**Вивід:**
```
CSV файл успішно оброблений.
```

## Поглиблений аналіз
CSV дебютував у 1970-х, як спосіб обміну даними між різними комп'ютерними системами. Є альтернативи як JSON або XML, але CSV залишається популярним через свою простоту. Розбір і запис CSV у TypeScript часто потребують сторонніх бібліотек, як-от ‘fast-csv’ для ефективного управління даними.

## Дивіться також
- [fast-csv documentation](https://c2fo.io/fast-csv/docs/introduction/getting-started)
- [Node.js fs module documentation](https://nodejs.org/api/fs.html)
- [RFC 4180](https://tools.ietf.org/html/rfc4180): CSV формат стандарт
- [Papaparse](https://www.papaparse.com/): Ще одна популярна бібліотека для парсингу CSV у веб-застосунках