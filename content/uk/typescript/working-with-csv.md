---
title:                "Робота з csv"
html_title:           "TypeScript: Робота з csv"
simple_title:         "Робота з csv"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/working-with-csv.md"
---

{{< edit_this_page >}}

## Що та навіщо?

Робота з форматом CSV - це один з важливих етапів в програмуванні, який дозволяє отримати та обробити дані з файлів у форматі CSV (Comma Separated Values - значення, розділені комами). Це особливо корисно для роботи з великими об'ємами даних, такими як бази даних або таблиці. Програмісти використовують CSV, щоб швидко та ефективно обробляти дані та працювати з ними на зручних для себе платформах.

## Як це зробити:

```TypeScript
import fs from 'fs';
import csv from 'csv-parser';

// Читаємо CSV файл та перетворюємо його дані в масив об'єктів
fs.createReadStream('дані.csv')
  .pipe(csv())
  .on('data', (row) => {
    console.log(row);
  })
  .on('end', () => {
    console.log('Робота з CSV завершена.');
  });
```
Вищеприведений приклад показує, як можна за допомогою модулів fs та csv-parser прочитати CSV файл та перетворити його дані в масив об'єктів, щоб легше з ними працювати. Результатом буде виведення серії об'єктів у консолі. Також можна використовувати інші модулі, наприклад papaparse, для роботи з CSV у TypeScript.

## Занурення у тему:

Формат CSV був створений в кінці 20-го століття і вирішував проблему обміну багаторівневими даними між різними системами. Існують інші альтернативи для роботи з даними, такі як JSON або XML, проте CSV є більш простим та ефективним для обробки великих об'ємів даних. Для реалізації зчитування та запису даних у CSV форматі, можна використовувати різні методи та інструменти, такі як парсери або модулі в рамках TypeScript.

## Дивіться також:

1. [Модуль fs в Node.js](https://nodejs.org/api/fs.html)
2. [csv-parser на GitHub](https://github.com/ryu1kn/csv-parser)
3. [papaparse на GitHub](https://github.com/mholt/PapaParse)