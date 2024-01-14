---
title:                "TypeScript: עבודה עם קובץ csv"
simple_title:         "עבודה עם קובץ csv"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/working-with-csv.md"
---

{{< edit_this_page >}}

## למה?

קבצי CSV הם אחת מן הפורמטים הנפוצים ביותר לשמירת נתונים מקובצי טקסט פשוטים. לעומת זאת, התקומפות הערכי של נתונים בקבצי CSV היא נמוכה כך שתמיד יש צורך לעבוד ולערוך אותם. בעזרת TypeScript ניתן ליצור תוכניות יעילות ומהירות לעבודה עם קבצי CSV ולעבוד בקלות ובקלות עם הנתונים שבתוכם.

## איך לעשות זאת?

הנה כמה דוגמאות לשימוש בקוד TypeScript לעבודה עם קבצי CSV:

```TypeScript
// ייבא את הספריה הדרושה
import * as fs from 'fs';
import * as csv from 'csv-parser';

// קרא את הקובץ והצג את התוכן שלו
fs.createReadStream('data.csv')
  .pipe(csv())
  .on('data', (row) => {
    console.log(row);
  })
  .on('end', () => {
    console.log('התוכן נקרא בהצלחה!');
  });
```

```TypeScript
// כתוב נתונים לקובץ CSV
import * as fs from 'fs';
import * as objectsToCsv from 'objects-to-csv';

// יצור מערך של אובייקטים עם נתונים
const data = [
  { name: 'איתי', age: 32 },
  { name: 'נעם', age: 28 },
  { name: 'ליאת', age: 26 }
];

// לכתוב נתונים לקובץ
const csv = new ObjectsToCsv(data);
await csv.toDisk('data.csv');
```

Output:

| name     | age |
| -------- | --- |
| איתי    | 32  |
| נעם     | 28  |
| ליאת    | 26  |

## מקורז

עבודה עם CSV יכולה להיות מורכבת ומוסיפה נסיבות מיוחדות. למעלה מהדוגמאות שבדקנו לא יכולות להתעלם ממקרים ייחודיים כמו CSV עם שורות ריקות, ערכים מיוחדים או תבניות מורכבות. כדי להתמודד עם זה, יש להשתמש בספריות נוספות כמו "Csv-Parser" או "Objects-To-Csv" אשר כולן ניתנות להתקנה דרך הפקודה `npm install`.

## ראה גם

- [מדריך לעבודה עם קבצי CSV ב TypeScript](