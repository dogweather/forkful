---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:49.226104-07:00
description: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 CSV (Comma-Separated Values,\
  \ \u0437\u043D\u0430\u0447\u0435\u043D\u043D\u044F, \u0440\u043E\u0437\u0434\u0456\
  \u043B\u0435\u043D\u0456 \u043A\u043E\u043C\u0430\u043C\u0438) \u0432\u043A\u043B\
  \u044E\u0447\u0430\u0454 \u0447\u0438\u0442\u0430\u043D\u043D\u044F \u0437 \u0456\
  \ \u0437\u0430\u043F\u0438\u0441 \u0443 \u0444\u0430\u0439\u043B\u0438 CSV, \u044F\
  \u043A\u0456 \u0454 \u043F\u043E\u0448\u0438\u0440\u0435\u043D\u0438\u043C \u0444\
  \u043E\u0440\u043C\u0430\u0442\u043E\u043C \u043E\u0431\u043C\u0456\u043D\u0443\
  \ \u0434\u0430\u043D\u0438\u043C\u0438, \u0449\u043E\u2026"
lastmod: '2024-02-25T18:49:46.388731-07:00'
model: gpt-4-0125-preview
summary: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 CSV (Comma-Separated Values,\
  \ \u0437\u043D\u0430\u0447\u0435\u043D\u043D\u044F, \u0440\u043E\u0437\u0434\u0456\
  \u043B\u0435\u043D\u0456 \u043A\u043E\u043C\u0430\u043C\u0438) \u0432\u043A\u043B\
  \u044E\u0447\u0430\u0454 \u0447\u0438\u0442\u0430\u043D\u043D\u044F \u0437 \u0456\
  \ \u0437\u0430\u043F\u0438\u0441 \u0443 \u0444\u0430\u0439\u043B\u0438 CSV, \u044F\
  \u043A\u0456 \u0454 \u043F\u043E\u0448\u0438\u0440\u0435\u043D\u0438\u043C \u0444\
  \u043E\u0440\u043C\u0430\u0442\u043E\u043C \u043E\u0431\u043C\u0456\u043D\u0443\
  \ \u0434\u0430\u043D\u0438\u043C\u0438, \u0449\u043E\u2026"
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 CSV"
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
