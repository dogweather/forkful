---
title:                "Работа с CSV"
aliases: - /ru/typescript/working-with-csv.md
date:                  2024-01-29T00:04:05.925507-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/typescript/working-with-csv.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и зачем?

Работа с CSV (значения, разделённые запятыми) подразумевает чтение и запись данных в текстовом формате, где каждая строка является записью данных, а поля разделены запятыми. Программисты используют CSV за его простоту и широкую поддержку среди инструментов для обмена данными.

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
