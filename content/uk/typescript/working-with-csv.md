---
title:                "Робота з CSV"
html_title:           "TypeScript: Робота з CSV"
simple_title:         "Робота з CSV"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/working-with-csv.md"
---

{{< edit_this_page >}}

## Чому

Написання програм, які працюють з CSV-файлами, є важливою задачею для багатьох програмістів. CSV-формат дозволяє легко зберігати, обробляти та передавати дані в табличному вигляді, що робить його дуже популярним для роботи з даними.

## Як працювати з CSV в TypeScript

Бібліотека `csv-parser` дозволяє легко читати та обробляти CSV-файли в TypeScript. Спочатку встановіть бібліотеку за допомогою `npm install csv-parser` та імпортуйте її у свій проект:

```TypeScript
import * as csv from 'csv-parser';
```

Потім, можна використовувати функцію `csv()` для зчитування CSV-файлу та перетворення його у об'єкти JavaScript:

```TypeScript
fs.createReadStream('data.csv')
  .pipe(csv())
  .on('data', (data) => console.log(data))
  .on('end', () => console.log('Читання завершено'));
```

В результаті, кожен рядок CSV-файлу буде представлений у вигляді об'єкту з ключами, які відповідають заголовкам стовпців.

## Поглиблене дослідження

Крім бібліотеки `csv-parser`, існують інші варіанти для роботи з CSV-файлами в TypeScript, такі як `fast-csv` та `papaparse`. Також, можна використовувати вбудовані засоби Node.js, щоб читати і записувати CSV-файли. Більше деталей про роботу з CSV-файлами можна знайти у [документації Node.js](https://nodejs.org/api/fs.html#fs_csv_files).

## Дивіться також

- [Документація `csv-parser`](https://csv.js.org/parse/)
- [Бібліотека `fast-csv`](https://github.com/C2FO/fast-csv)
- [Бібліотека `papaparse`](https://www.papaparse.com/)