---
title:                "עובדים עם CSV"
date:                  2024-02-03T19:21:50.075934-07:00
model:                 gpt-4-0125-preview
simple_title:         "עובדים עם CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

עבודה עם CSV (ערכים מופרדים בפסיקים) כוללת קריאה מקבצי CSV וכתיבה אליהם, פורמט חליפין נתונים נפוץ המשמש בשל פשטותו והתמיכה הרחבה במגוון פלטפורמות ושפות. מתכנתים עוסקים בקבצי CSV כדי לייבא או לייצא נתונים מיישומים, מסדי נתונים ושירותים, ובכך מאפשרים מניפולציה ושיתוף נתונים בקלות.

## איך לעשות:

ב-TypeScript, ניתן לעבוד עם קבצי CSV באמצעות קוד ייחודי או על ידי שימוש בספריות צד שלישי כמו `csv-parser` לקריאה ו-`csv-writer` לכתיבה של קבצי CSV.

### קריאת CSV עם `csv-parser`

ראשית, התקן את `csv-parser` באמצעות npm:

```
npm install csv-parser
```

לאחר מכן, קרא קובץ CSV כך:

```typescript
import fs from 'fs';
import csv from 'csv-parser';

const results = [];

fs.createReadStream('data.csv')
  .pipe(csv())
  .on('data', (data) => results.push(data))
  .on('end', () => {
    console.log(results);
    // פלט: מערך של אובייקטים, כל אחד מייצג שורה ב-CSV
  });
```

בהנחה ש-`data.csv` מכיל:

```
name,age
Alice,30
Bob,25
```

הפלט יהיה:

```
[ { name: 'Alice', age: '30' }, { name: 'Bob', age: '25' } ]
```

### כתיבת CSV עם `csv-writer`

כדי לכתוב לקובץ CSV, התקן ראשית את `csv-writer`:

```
npm install csv-writer
```

לאחר מכן, השתמש בו כך:

```typescript
import { createObjectCsvWriter as createCsvWriter } from 'csv-writer';

const csvWriter = createCsvWriter({
  path: 'out.csv',
  header: [
    {id: 'name', title: 'NAME'},
    {id: 'age', title: 'AGE'}
  ]
});

const data = [
  { name: 'Alice', age: 30 },
  { name: 'Bob', age: 25 }
];

csvWriter
  .writeRecords(data)
  .then(() => console.log('The CSV file was written successfully'));
```

קוד זה כותב את הבא ל-`out.csv`:

```
NAME,AGE
Alice,30
Bob,25
```

הדוגמאות אלו מראות כיצד לשלב עיבוד CSV בפרויקטים של TypeScript שלכם באופן יעיל, בין אם מדובר בקריאת נתונים לניתוח או בשמירת נתוני אפליקציה בחוץ.
