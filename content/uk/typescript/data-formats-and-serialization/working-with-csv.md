---
title:                "Робота з CSV"
aliases:
- /uk/typescript/working-with-csv.md
date:                  2024-02-03T19:21:49.226104-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та Чому?

Робота з CSV (Comma-Separated Values, значення, розділені комами) включає читання з і запис у файли CSV, які є поширеним форматом обміну даними, що використовується через їх простоту та широку підтримку на різних платформах та мовах. Програмісти займаються файлами CSV для імпорту або експорту даних з програм, баз даних і сервісів, що дає змогу легко маніпулювати даними та ділитися ними.

## Як це зробити:

У TypeScript ви можете працювати з файлами CSV за допомогою нативного коду або використовуючи сторонні бібліотеки, як-от `csv-parser` для читання та `csv-writer` для запису файлів CSV.

### Читання CSV за допомогою `csv-parser`

Спочатку встановіть `csv-parser` через npm:

```
npm install csv-parser
```

Потім прочитайте файл CSV таким чином:

```typescript
import fs from 'fs';
import csv from 'csv-parser';

const results = [];

fs.createReadStream('data.csv')
  .pipe(csv())
  .on('data', (data) => results.push(data))
  .on('end', () => {
    console.log(results);
    // Вивід: Масив об'єктів, кожен з яких представляє рядок у файлі CSV
  });
```

Припустимо, `data.csv` містить:

```
name,age
Alice,30
Bob,25
```

Вивід буде:

```
[ { name: 'Alice', age: '30' }, { name: 'Bob', age: '25' } ]
```

### Запис CSV за допомогою `csv-writer`

Щоб записати у файл CSV, спочатку встановіть `csv-writer`:

```
npm install csv-writer
```

Потім використовуйте його так:

```typescript
import { createObjectCsvWriter as createCsvWriter } from 'csv-writer';

const csvWriter = createCsvWriter({
  path: 'out.csv',
  header: [
    {id: 'name', title: 'ІМ'Я'},
    {id: 'age', title: 'ВІК'}
  ]
});

const data = [
  { name: 'Alice', age: 30 },
  { name: 'Bob', age: 25 }
];

csvWriter
  .writeRecords(data)
  .then(() => console.log('Файл CSV було успішно записано'));
```

Цей код записує наступне у `out.csv`:

```
ІМ'Я,ВІК
Alice,30
Bob,25
```

Ці приклади показують, як ефективно інтегрувати обробку даних CSV у ваші проекти на TypeScript, чи то для аналізу даних, чи для зберігання даних програми зовні.
