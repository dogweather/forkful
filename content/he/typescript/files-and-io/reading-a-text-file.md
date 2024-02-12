---
title:                "קריאת קובץ טקסט"
aliases:
- /he/typescript/reading-a-text-file.md
date:                  2024-01-20T17:55:33.353282-07:00
model:                 gpt-4-1106-preview
simple_title:         "קריאת קובץ טקסט"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
קריאת קובץ טקסט היא פעולה שבה אנו מפענחים תוכן מקובץ טקסט וטוענים אותו לתוך התוכנה שלנו. תוכניתנים עושים זאת כדי לטעון נתונים, תצורות, או סקריפטים שהוזנו מחוץ לקוד המקור.

## איך לעשות:
ב-TS אנו נשתמש ב-Node.js המובנה `fs`. כדי לקרוא טקסט:

```TypeScript
import { readFileSync } from 'fs';

const readTextFile = (filePath: string): string => {
    try {
        const text = readFileSync(filePath, 'utf8');
        return text;
    } catch (error) {
        throw new Error(`Could not read the file: ${error.message}`);
    }
};

console.log(readTextFile('./example.txt'));
```

פלט לדוגמה:
```
זהו קובץ טקסט דוגמתי.
```

## ניתוח עמוק
קריאת קבצים היא חלק חשוב בתיכנות מאז שהמחשבים הראשונים הופצו. בעידן המודרני, ספריות כמו `fs` מאפשרות לנו לבצע זאת בקלות. ישנם שני גישות עיקריות: סנכרוני (כמו בדוגמה) ואסינכרוני, שבו אנו משתמשים בפונקציות כמו `fs.readFile`. ישנן סיבות להשתמש בכל אחת וכן כלים בצופן רשת אחרים כמו `axios` לקריאת נתוני טקסט מהאינטרנט.

## ראה גם
- [מדריך fs רשמי של Node.js](https://nodejs.org/api/fs.html)
- [ניהול קבצים אסינכרוני ב-JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Using_promises#working_with_file_systems)
