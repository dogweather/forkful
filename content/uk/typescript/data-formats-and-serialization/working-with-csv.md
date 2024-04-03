---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:49.226104-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0423 TypeScript \u0432\u0438 \u043C\u043E\u0436\u0435\u0442\u0435 \u043F\u0440\
  \u0430\u0446\u044E\u0432\u0430\u0442\u0438 \u0437 \u0444\u0430\u0439\u043B\u0430\
  \u043C\u0438 CSV \u0437\u0430 \u0434\u043E\u043F\u043E\u043C\u043E\u0433\u043E\u044E\
  \ \u043D\u0430\u0442\u0438\u0432\u043D\u043E\u0433\u043E \u043A\u043E\u0434\u0443\
  \ \u0430\u0431\u043E \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\
  \u0443\u044E\u0447\u0438 \u0441\u0442\u043E\u0440\u043E\u043D\u043D\u0456 \u0431\
  \u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0438, \u044F\u043A-\u043E\u0442\
  \ `csv-parser` \u0434\u043B\u044F\u2026"
lastmod: '2024-03-13T22:44:48.903881-06:00'
model: gpt-4-0125-preview
summary: "\u0423 TypeScript \u0432\u0438 \u043C\u043E\u0436\u0435\u0442\u0435 \u043F\
  \u0440\u0430\u0446\u044E\u0432\u0430\u0442\u0438 \u0437 \u0444\u0430\u0439\u043B\
  \u0430\u043C\u0438 CSV \u0437\u0430 \u0434\u043E\u043F\u043E\u043C\u043E\u0433\u043E\
  \u044E \u043D\u0430\u0442\u0438\u0432\u043D\u043E\u0433\u043E \u043A\u043E\u0434\
  \u0443 \u0430\u0431\u043E \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\
  \u0432\u0443\u044E\u0447\u0438 \u0441\u0442\u043E\u0440\u043E\u043D\u043D\u0456\
  \ \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0438, \u044F\u043A-\u043E\
  \u0442 `csv-parser` \u0434\u043B\u044F \u0447\u0438\u0442\u0430\u043D\u043D\u044F\
  \ \u0442\u0430 `csv-writer` \u0434\u043B\u044F \u0437\u0430\u043F\u0438\u0441\u0443\
  \ \u0444\u0430\u0439\u043B\u0456\u0432 CSV."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 CSV"
weight: 37
---

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
