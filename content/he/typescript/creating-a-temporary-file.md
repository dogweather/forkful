---
title:                "יצירת קובץ זמני"
html_title:           "C#: יצירת קובץ זמני"
simple_title:         "יצירת קובץ זמני"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# כיצד ליצור קובץ זמני ב- TypeScript 

## מה זה? ולמה?

יצירת קובץ זמני היא קובץ שנמחק לאחר שהמשימה שלו מושלמת. מתכנתים משתמשים בקבצים זמניים כדי לאחסן נתונים באופן זמני לצרכי עיבוד, גיבוי או העברה.

## איך לעבוד עם זה:

הנה דוגמה על איך ליצור קובץ זמני ב- TypeScript:

```TypeScript
const fs = require('fs');
const tmp = require('tmp');

let tmpobj = tmp.fileSync({ template: 'tmp-XXXXXX.txt' });

fs.writeSync(tmpobj.fd, "Hello World!");

console.log("File: ", tmpobj.name);
console.log("File content: ", fs.readFileSync(tmpobj.name, 'utf8'));
```

עם הקוד הזה, אנו יוצרים קובץ זמני וכותבים לו "שלום עולם!". לאחר מכן, אנו מדפיסים את שם הקובץ ואת התוכן שלו.

## צלילה עמוקה:

אם אנחנו באמת רוצים להבין את זה:
1. ההקשר ההיסטורי - קבצים זמניים הם ממשק עם מערכת ההפעלה שנוצר בשנות ה-60 של המאה ה-20.
2. אלטרנטיבות - ניתן לשמור נתונים במאגר מידע או בזיכרון ה- RAM של המחשב.
3. פרטי המימוש - קבצים זמניים מבוססים על איך מערכת ההפעלה מנהלת את הקבצים. זה יהיה שונה בין מערכות ההפעלה השונות.


## ראו גם:

1. בדוק את מסמכי ה- API של 'fs' ו- 'tmp' באתר הרשמי של Node.js: 
https://nodejs.org/api/fs.html
https://www.npmjs.com/package/tmp

2. עיין בסדרת הטוטוריאלים של Mozilla על ג'אווהסקריפט:
https://developer.mozilla.org/he/docs/Web/JavaScript

3. קרא את הספר "You Don't Know JS" של Kyle Simpson:
https://github.com/getify/You-Dont-Know-JS

אז זהו, עכשיו אתה יודע איך ליצור קובץ זמני ב- TypeScript.מקווה שזה היה שימושי!