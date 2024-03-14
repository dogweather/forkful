---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:50.075934-07:00
description: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD CSV (\u05E2\u05E8\u05DB\u05D9\
  \u05DD \u05DE\u05D5\u05E4\u05E8\u05D3\u05D9\u05DD \u05D1\u05E4\u05E1\u05D9\u05E7\
  \u05D9\u05DD) \u05DB\u05D5\u05DC\u05DC\u05EA \u05E7\u05E8\u05D9\u05D0\u05D4 \u05DE\
  \u05E7\u05D1\u05E6\u05D9 CSV \u05D5\u05DB\u05EA\u05D9\u05D1\u05D4 \u05D0\u05DC\u05D9\
  \u05D4\u05DD, \u05E4\u05D5\u05E8\u05DE\u05D8 \u05D7\u05DC\u05D9\u05E4\u05D9\u05DF\
  \ \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05E0\u05E4\u05D5\u05E5 \u05D4\u05DE\u05E9\
  \u05DE\u05E9 \u05D1\u05E9\u05DC \u05E4\u05E9\u05D8\u05D5\u05EA\u05D5 \u05D5\u05D4\
  \u05EA\u05DE\u05D9\u05DB\u05D4 \u05D4\u05E8\u05D7\u05D1\u05D4 \u05D1\u05DE\u05D2\
  \u05D5\u05D5\u05DF \u05E4\u05DC\u05D8\u05E4\u05D5\u05E8\u05DE\u05D5\u05EA \u05D5\
  \u05E9\u05E4\u05D5\u05EA.\u2026"
lastmod: '2024-03-13T22:44:38.954737-06:00'
model: gpt-4-0125-preview
summary: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD CSV (\u05E2\u05E8\u05DB\u05D9\
  \u05DD \u05DE\u05D5\u05E4\u05E8\u05D3\u05D9\u05DD \u05D1\u05E4\u05E1\u05D9\u05E7\
  \u05D9\u05DD) \u05DB\u05D5\u05DC\u05DC\u05EA \u05E7\u05E8\u05D9\u05D0\u05D4 \u05DE\
  \u05E7\u05D1\u05E6\u05D9 CSV \u05D5\u05DB\u05EA\u05D9\u05D1\u05D4 \u05D0\u05DC\u05D9\
  \u05D4\u05DD, \u05E4\u05D5\u05E8\u05DE\u05D8 \u05D7\u05DC\u05D9\u05E4\u05D9\u05DF\
  \ \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05E0\u05E4\u05D5\u05E5 \u05D4\u05DE\u05E9\
  \u05DE\u05E9 \u05D1\u05E9\u05DC \u05E4\u05E9\u05D8\u05D5\u05EA\u05D5 \u05D5\u05D4\
  \u05EA\u05DE\u05D9\u05DB\u05D4 \u05D4\u05E8\u05D7\u05D1\u05D4 \u05D1\u05DE\u05D2\
  \u05D5\u05D5\u05DF \u05E4\u05DC\u05D8\u05E4\u05D5\u05E8\u05DE\u05D5\u05EA \u05D5\
  \u05E9\u05E4\u05D5\u05EA.\u2026"
title: "\u05E2\u05D5\u05D1\u05D3\u05D9\u05DD \u05E2\u05DD CSV"
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
