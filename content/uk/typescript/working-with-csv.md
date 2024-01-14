---
title:                "TypeScript: Робота з csv"
simple_title:         "Робота з csv"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/working-with-csv.md"
---

{{< edit_this_page >}}

## Чому

Робота з CSV - це необхідна уміння для багатьох програмістів, оскільки цей формат є одним з найпоширеніших для обміну даними між різними додатками. Навчитися працювати з CSV дозволить вам легко і зручно обробляти великі об'єми даних і експортувати їх у форматі, який може зрозуміти будь-який користувач.

## Як

Найпростіший спосіб прочитати CSV файл і отримати дані є використання вбудованої бібліотеки Node.js `fs`.

```TypeScript
import * as fs from 'fs';

// Відкриваємо файл для зчитування
fs.readFile('file.csv', (err, data) => {
  if(!err) {
    // Перетворюємо буфер даних у рядок
    const csv = data.toString();
    // Ділимо рядок на рядки
    const lines = csv.split('\n');
    // Виводимо перший рядок
    console.log(lines[0]);
  }
});
```

Якщо потрібно парсити значення, можна використати бібліотеку `csv-parser`.

```TypeScript
import * as fs from 'fs';
import * as csvParser from 'csv-parser';

// Відкриваємо файл для зчитування
fs.createReadStream('file.csv')
  // Передаємо його в \`csv-parser\`
  .pipe(csvParser())
  // Викликаємо колбек функцію для кожного рядка
  .on('data', (row) => {
    // Виводимо значення колонки "name"
    console.log(row.name);
  })
  // При завершенні обробки виводимо повідомлення
  .on('end', () => {
    console.log('Файл успішно оброблено');
  });
```

## Глибший розгляд

При роботі з CSV файлами, варто приділяти увагу додатковим настройкам, таким як розділювач колонок і наявність заголовків рядків. Також важливо знати, що рядки з можуть містити помилки, які необхідно обробляти окремо. Для цього рекомендується використовувати бібліотеку `csv-parse`.

Для експорту даних у CSV формат також можна використовувати вбудовану `fs` бібліотеку або бібліотеку `fast-csv` для ефективного формування великих файлів.

## Дивіться також

- [Документація Node.js `fs` бібліотеки](https://nodejs.org/api/fs.html)
- [Документація `csv-parser` бібліотеки](https://csv.js.org/)
- [Документація `csv-parse` бібліотеки](https://csv.js.org/parse/)
- [Документація `fast-csv` бібліотеки](https://github.com/C2FO/fast-csv)