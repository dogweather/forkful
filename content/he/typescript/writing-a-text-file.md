---
title:                "כתיבת קובץ טקסט"
html_title:           "TypeScript: כתיבת קובץ טקסט"
simple_title:         "כתיבת קובץ טקסט"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבת קובץ טקסט הוא תהליך הכולל כתיבה של מידע טקסטואלי לתוך קובץ מחשב. תהליך זה נעשה למטרה של שמירת מידע בצורה שהוא יוכל להיות ביצועי לאורך זמן רב יותר וגם להפעלת פעולות נוספות עליו כגון קריאה, עריכה ומחיקה.

## איך לעשות זאת?
אלו הם כמה דוגמאות לתיעוד `TypeScript` לפתרונות לכתיבת קובץ טקסט והפלט שנוצר:

```TypeScript 
// כתיבת טקסט לקובץ קיים
const fs = require('fs'); 

fs.writeFile('example.txt', 'הפתרון שלי', function (err) { 
    if (err) throw err; 
    console.log('הקובץ נוצר!');
});

// כתיבת טקסט חדש לקובץ
import { writeFile } from 'fs'; 

writeFile('new-example.txt', 'טקסט חדש כאן', (err) => { 
    if (err) throw err; 
    console.log('הקובץ החדש נוצר!');
});
```
### פלט:
example.txt: הפתרון שלי 
new-example.txt: טקסט חדש כאן 

## חפירה עמוקה:
תהליך כתיבת קובץ טקסט הוא די יסודי ונמצא כבר כמה עשורים בעולם התכנות. תחילה, פתרון נתמך את כתיבת קבצים לפתרונות ממסד נתונים כמו MySQL, אך הוא גם עובד על קבצים סטטיים. אם אתה מחפש פתרונות עוד יעילים, ניתן להשתמש בפתרונות ג כמו שירותי הענן הקיימים כיום (AWS, GCP ועוד).

## ראה גם:
למידע נוסף על כתיבת קבצים טקסט והפעלת פקודות נוספות על קבצים, ראה את המדריך המפורט הזה מאת Microsoft: https://docs.microsoft.com/en-us/windows/win32/fileio/creating-writes. ניתן גם ליצור קשר עם קהילת המפתחים של TypeScript על מנת לחלוק רעיונות וסודות נפיצה: https://www.typescriptlang.org/docs/home.html.