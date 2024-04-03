---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:05.925507-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0427\u0442\u0435\u043D\u0438\u0435 CSV \u0432 TypeScript \u043F\u0440\
  \u043E\u0441\u0442\u043E \u0441 \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\
  \u043A\u0430\u043C\u0438, \u0442\u0430\u043A\u0438\u043C\u0438 \u043A\u0430\u043A\
  \ `papaparse`. \u0414\u043B\u044F \u0440\u0430\u0431\u043E\u0442\u044B \u0441 CSV-\u0444\
  \u0430\u0439\u043B\u0430\u043C\u0438 \u0441\u043D\u0430\u0447\u0430\u043B\u0430\
  \ \u0443\u0441\u0442\u0430\u043D\u043E\u0432\u0438\u0442\u0435 \u0435\u0451."
lastmod: '2024-03-13T22:44:44.629447-06:00'
model: gpt-4-0125-preview
summary: "\u0427\u0442\u0435\u043D\u0438\u0435 CSV \u0432 TypeScript \u043F\u0440\u043E\
  \u0441\u0442\u043E \u0441 \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\
  \u0430\u043C\u0438, \u0442\u0430\u043A\u0438\u043C\u0438 \u043A\u0430\u043A `papaparse`."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 CSV"
weight: 37
---

## Как это сделать:
Чтение CSV в TypeScript просто с библиотеками, такими как `papaparse`. Для работы с CSV-файлами сначала установите её:

```bash
npm install papaparse
```

Вот как прочитать CSV-файл:

```typescript
import * as fs from 'fs';
import * as Papa from 'papaparse';

const csvFilePath = 'path/to/your/file.csv';
const fileContent = fs.readFileSync(csvFilePath, 'utf8');

Papa.parse(fileContent, {
  complete: (result) => {
    console.log(result.data);
  }
});
```

Для записи в CSV вы можете использовать `csv-writer`. Установите его так:

```bash
npm install csv-writer
```

А затем запишите в CSV-файл так:

```typescript
import * as createCsvWriter from 'csv-writer';

const csvWriter = createCsvWriter.createObjectCsvWriter({
  path: 'path/to/your/output.csv',
  header: [
    {id: 'name', title: 'NAME'},
    {id: 'age', title: 'AGE'}
  ]
});

const data = [
  { name: 'John', age: 28 },
  { name: 'Jane', age: 32 }
];

csvWriter.writeRecords(data)
  .then(() => console.log('В CSV-файл успешно записаны данные.'));
```

Вывод в файле 'output.csv' будет следующим:

```
NAME,AGE
John,28
Jane,32
```

## Погружение
CSV является основой в обмене данными с ранних эпох компьютерной эры благодаря его читабельности и простоте. Он не лишён проблем; например, отсутствие стандартизации может привести к ошибкам разбора. Альтернативы, такие как JSON и XML, предлагают более сложные структуры и типы данных. При реализации парсеров/писателей CSV следует учитывать кодировку символов и правильную обработку специальных символов, чтобы избежать ошибок.

## См. также
- Документация `papaparse`: [Papa Parse - мощный парсер CSV](https://www.papaparse.com/)
- Документация `csv-writer`: [CSV Writer - запись файлов CSV для Node](https://csv.js.org/)
- Для более глубокого технического понимания документ RFC 4180 предлагает де-факто стандарт для форматов CSV: [RFC 4180](https://tools.ietf.org/html/rfc4180)
- Для сравнения форматов файлов см.: [JSON против XML против CSV](https://www.geeksforgeeks.org/difference-between-json-and-xml/)
