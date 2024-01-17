---
title:                "עובדים עם csv"
html_title:           "TypeScript: עובדים עם csv"
simple_title:         "עובדים עם csv"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/working-with-csv.md"
---

{{< edit_this_page >}}

## מה ולמה?

עבודה עם קבצי CSV היא חלק חשוב של פיתוח תוכנה. קובצי CSV הם קבצים שמכילים נתונים מסודרים בתווים מפרידים, כגון פסיקים או טאבים. תוכניות פיתוח נהוגות לגשת לנתונים בקבצי CSV כדי ליצור תוכניות איכותיות ויעילות.

## איך לעשות:

#### קריאה של קובץ CSV:

```TypeScript
import fs from 'fs';
import csv from 'csv-parser';

fs.createReadStream('file.csv')
    .pipe(csv())
    .on('data', (row) => {
        console.log(row)
    })
    .on('end', () => {
        console.log('CSV file successfully processed.');
    });
```

#### כתיבת קובץ CSV:

```TypeScript
import fs from 'fs';
import csvWriter from 'csv-writer';

const data = [
    { name: 'John', age: '30' },
    { name: 'Jane', age: '25' },
];

const writer = csvWriter.createObjectCsvWriter({
    path: 'file.csv',
    header: [
        { id: 'name', title: 'Name' },
        { id: 'age', title: 'Age' }
    ]
});

writer.writeRecords(data)
    .then(() => {
        console.log('CSV file successfully created.');
    });
```

### מילוי נתונים בקובץ CSV:

```TypeScript
import fs from 'fs';
import csvWriter from 'csv-writer';

fs.createReadStream('file.csv')
    .pipe(csv())
    .on('data', (row) => {
        const newData = {...row, job: 'Developer'};
        writer.writeRecords([newData])
            .then(() => {
                console.log('New data successfully added to CSV file.');
            });
    })
    .on('end', () => {
        console.log('CSV file successfully processed.');
    });
```

## טיול עמוק:

קבצי CSV נוצרו כדי לאפשר למחשבים לתקשר עם גישות מאחורי הקלעים. בעבר כאשר נתונים נמצאים בגישת מאחורי הקלעים, התוכנה צריכה להיתקל בבעיות קשורות לעברית וגבים בגלל שגישת מאחורי הקלעים לא נותן את השפה בנפרד. כיום, קבצי CSV משמשים ככלי עבור פיתוחתעויהי ואנשי עסקים מכיוון שהם מספקים יעילות ויישומיות.

אלטרנטיבה נפוצה לקבצי CSV הוא שימוש במסדי נתונים, אך לפעמים קבצי CSV יכולים להיות יעילים יותר עבור פרויקטים קטנים שאינם מחייבים מסד נתונים מסובך.

לעתים קרובות, קבצי CSV מתיימרים להיות מסובכים עבור תוכנות פיתוח מורכבות מאשר יישומים פשוטים. במקרה כזה, במקום לטפל בנתונים ישירות בקוד, יש לשקול את שימוש בספריות וכלים נוספים כדי לעבד קבצי CSV בצורה מתאימה יותר. 

## ראה גם:

- [ספריית פיתוח CSV עבור TypeScript](https://www.npmjs.com/package/csv)
- [כלי כתיבת CSV עבור TypeScript](https://www.npmjs.com/package/csv-writer)