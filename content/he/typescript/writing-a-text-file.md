---
title:                "כתיבה לקובץ טקסט"
date:                  2024-01-19
html_title:           "Bash: כתיבה לקובץ טקסט"
simple_title:         "כתיבה לקובץ טקסט"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבת קובץ טקסט היא תהליך שבו נוצר קובץ חדש או מעודכן עם נתוני טקסט. תכניתנים עושים זאת לשמירת נתונים, לוגים, או סתם לתחזוקת מידע באופן קריא ופשוט.

## איך לעשות:
```TypeScript
import { writeFile } from 'fs';

const fileName = 'הודעה.txt';
const content = 'שלום עולם!';

writeFile(fileName, content, 'utf8', (err) => {
  if (err) {
    console.error('הייתה שגיאה בכתיבת הקובץ:', err);
  } else {
    console.log(`הקובץ ${fileName} נכתב בהצלחה.`);
  }
});
```
פלט:
```
הקובץ הודעה.txt נכתב בהצלחה.
```

## שיטות אחרות:
בעבר, נעשה שימוש ב-API הסינכרוני של נוד לכתיבת קבצים, אך הרגלים אלו הופכים לפחות נפוצים עקב המעבר לקוד אסינכרוני שמאפשר עיבוד יעיל יותר של פעולות I/O. יש שיטות כמו streams שישמשו לכתיבת קבצים גדולים או לזרימה אצל קובץ שנכתב.

## ראה גם:
- [מודול fs של Node.js](https://nodejs.org/api/fs.html)
- [מדריך לעבודה עם Streams בNode.js](https://nodejs.org/api/stream.html)
- [מידע נוסף על Buffer ועבודה עם נתוני בינארי בNode.js](https://nodejs.org/api/buffer.html)
