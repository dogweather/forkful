---
date: 2024-01-20 17:55:33.353282-07:00
description: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\
  \u05E1\u05D8 \u05D4\u05D9\u05D0 \u05E4\u05E2\u05D5\u05DC\u05D4 \u05E9\u05D1\u05D4\
  \ \u05D0\u05E0\u05D5 \u05DE\u05E4\u05E2\u05E0\u05D7\u05D9\u05DD \u05EA\u05D5\u05DB\
  \u05DF \u05DE\u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\u05D8 \u05D5\u05D8\u05D5\
  \u05E2\u05E0\u05D9\u05DD \u05D0\u05D5\u05EA\u05D5 \u05DC\u05EA\u05D5\u05DA \u05D4\
  \u05EA\u05D5\u05DB\u05E0\u05D4 \u05E9\u05DC\u05E0\u05D5. \u05EA\u05D5\u05DB\u05E0\
  \u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA\
  \ \u05DB\u05D3\u05D9 \u05DC\u05D8\u05E2\u05D5\u05DF \u05E0\u05EA\u05D5\u05E0\u05D9\
  \u05DD, \u05EA\u05E6\u05D5\u05E8\u05D5\u05EA, \u05D0\u05D5 \u05E1\u05E7\u05E8\u05D9\
  \u05E4\u05D8\u05D9\u05DD \u05E9\u05D4\u05D5\u05D6\u05E0\u05D5\u2026"
lastmod: '2024-03-13T22:44:38.946585-06:00'
model: gpt-4-1106-preview
summary: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8 \u05D4\u05D9\u05D0 \u05E4\u05E2\u05D5\u05DC\u05D4 \u05E9\u05D1\u05D4 \u05D0\
  \u05E0\u05D5 \u05DE\u05E4\u05E2\u05E0\u05D7\u05D9\u05DD \u05EA\u05D5\u05DB\u05DF\
  \ \u05DE\u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\u05D8 \u05D5\u05D8\u05D5\u05E2\
  \u05E0\u05D9\u05DD \u05D0\u05D5\u05EA\u05D5 \u05DC\u05EA\u05D5\u05DA \u05D4\u05EA\
  \u05D5\u05DB\u05E0\u05D4 \u05E9\u05DC\u05E0\u05D5. \u05EA\u05D5\u05DB\u05E0\u05D9\
  \u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\
  \u05D3\u05D9 \u05DC\u05D8\u05E2\u05D5\u05DF \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD\
  , \u05EA\u05E6\u05D5\u05E8\u05D5\u05EA, \u05D0\u05D5 \u05E1\u05E7\u05E8\u05D9\u05E4\
  \u05D8\u05D9\u05DD \u05E9\u05D4\u05D5\u05D6\u05E0\u05D5\u2026"
title: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8"
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
