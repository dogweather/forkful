---
title:                "עובדים עם CSV"
html_title:           "Javascript: עובדים עם CSV"
simple_title:         "עובדים עם CSV"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/working-with-csv.md"
---

{{< edit_this_page >}}

## למה

טקסט כניסה כאן שמסביר מדוע זה מעניין לעבוד עם קבצי CSV ב-Javascript. ניתן לציין את התכונות המיוחדות של קבצי CSV והיכן הם נמצאים בשימוש.

## איך לעשות זאת

לאחרת קריאת הטקסט הזה תלמד איך לפתח תוכניות ב-Javascript שעובדות עם קבצי CSV. כמו כן, יוצגו דוגמאות קוד ופלט כדי להבין טוב יותר את התהליך.

```javascript
// לקרוא קובץ CSV עם ספריית Node.js 'fs'
const fs = require('fs');
const csv = require('csv-parser');

// קריאת קובץ CSV עם מתודת csv-parser
fs.createReadStream('data.csv')
    .pipe(csv())
    .on('data', (row) => {
        // כל שורה נתונים תהיה זמינה כאובייקט row
        console.log(row);
    })
    .on('end', () => {
        console.log('קריאת הקובץ CSV הסתיימה!');
    });
```

פלט:

```javascript
{
    עמודה1: 'ערך1',
    עמודה2: 'ערך2',
    עמודה3: 'ערך3' 
}
{
    עמודה1: 'ערך4',
    עמודה2: 'ערך5',
    עמודה3: 'ערך6' 
}
ערך7
```

## Deep Dive

העמקת ידע על עבודה עם קבצי CSV ב-Javascript ישלים את הידע ויאפשר לכם להתמודד עם כל מצבי השימוש האפשריים. כלי זה הנחשב כמקסים נמצא בשימוש לטיפול בנתונים ומאפשר להפיק נתונים מקבצי CSV באופן מהיר ויעיל.

כמו כן, כדאי לדעת שניתן לעבוד גם עם קבצי CSV ב-Javascript דרך הדפדפן שלכם באמצעות ספריות כמו PapaParse או D3.js.

## ראו גם

- [מדריכים לעבודה עם CSV ו-Javascript](https://hackernoon.com/working-with-csv-files-in-javascript-758a9ba0602a)
- [ספריית csv-parser ב-NPM](https://www.npmjs.com/package/csv-parser)
- [PapaParse ספריית קריאת נתונים מקבצי CSV בדפדפן](https://www.papaparse.com/)