---
date: 2024-01-20 17:55:33.353282-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1-TS \u05D0\u05E0\
  \u05D5 \u05E0\u05E9\u05EA\u05DE\u05E9 \u05D1-Node.js \u05D4\u05DE\u05D5\u05D1\u05E0\
  \u05D4 `fs`. \u05DB\u05D3\u05D9 \u05DC\u05E7\u05E8\u05D5\u05D0 \u05D8\u05E7\u05E1\
  \u05D8."
lastmod: '2024-03-13T22:44:38.946585-06:00'
model: gpt-4-1106-preview
summary: "\u05D1-TS \u05D0\u05E0\u05D5 \u05E0\u05E9\u05EA\u05DE\u05E9 \u05D1-Node.js\
  \ \u05D4\u05DE\u05D5\u05D1\u05E0\u05D4 `fs`."
title: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8"
weight: 22
---

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
