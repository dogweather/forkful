---
title:                "Робота з csv"
html_title:           "Javascript: Робота з csv"
simple_title:         "Робота з csv"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/working-with-csv.md"
---

{{< edit_this_page >}}

## Що & Чому?
CSV є одним з найвикористовуваніших форматів для обробки даних в програмуванні. Це текстовий формат, що представляє табличну структуру даних, де кожен рядок відповідає одному рядку таблиці, а значення зберігаються у вигляді коми-роздільних стовпців. Програмісти використовують CSV для ефективної обробки та зберігання даних.

## Як:
Використання CSV в Javascript є досить простим. Нижче представлено приклад коду, який виводить дані з CSV файлу у консоль:
```Javascript
const CSV = require('csv-parser');
const fs = require('fs');

fs.createReadStream('data.csv')
  .pipe(CSV())
  .on('data', (row) => {
    console.log(row);
  })
  .on('end', () => {
    console.log('Successfully read data from CSV file.');
  });
```
Вихідні дані будуть виведені у форматі об'єкта, де назви стовпців будуть використовуватися як ключі, а значення - як значення.

## В глибину:
CSV формат був створений у 1972 році і є стандартом для імпорту та експорту даних у багатьох програмах для обробки тексту та електронної таблиці. Існують альтернативні формати, такі як JSON та XML, але CSV переваги в простоті та доступності. У Javascript, CSV можна обробляти за допомогою різних бібліотек, таких як Papa Parse та CSV Parser.

## Дивись також:
- [Приклад роботи з CSV у JavaScript](https://www.codementor.io/kellystannard/javascript-csv-parser-how-to-parse-csv-files-in-javascript-du107rgla)
- [Документація по бібліотеці CSV Parser](https://www.npmjs.com/package/csv-parser)
- [Документація по бібліотеці Papa Parse](https://www.papaparse.com/)