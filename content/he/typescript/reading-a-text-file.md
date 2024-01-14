---
title:    "TypeScript: קריאת קובץ טקסט"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## למה

הקריאה של קובץ טקסט יכולה להיות כלי מאוד שימושי בפיתוח תוכנה בשפת TypeScript. לנו יכול לקרוא קבצי טקסט לשם קריאה ועיבוד מידע, כגון קבצי קונפיגורציה או נתוני קלט.

## איך לעשות זאת

ובכן, הנה דוגמה פשוטה לקריאת קובץ טקסט ב TypeScript:

```TypeScript
// ייבוא מודול הקריאה של קבצים
import * as fs from 'fs';

// הפעלת הפונקציה readFileSync לקריאת הקובץ במצב סינכרוני
const fileContent = fs.readFileSync('textfile.txt', 'utf-8');

// הדפסת תוכן הקובץ
console.log(fileContent);
```

✨ נצמד לקוד הדוגמה ונראה איך הפלט ישתנה בהתאם לתוכן של הקובץ המצוי בנתיב הנתון. נניח שהקובץ textfile.txt מכיל את המילים "שלום עולם". הפלט שנקבל יהיה "שלום עולם".

## דיב דייב

כעת, נעלה את המפתח הלבן ונפתח חלון לעולם העמוק של קריאת קבצי טקסט. קריאת קובץ טקסט ב TypeScript רואים שימוש רב בעולם התכנות, בעיקר כאמצעי לקריאת ועיבוד מידע מקבצים שונים. בנוסף, ישנן אפשרויות רבות להתאים את פונקציית הקריאה לצרכים שונים. ניתן לשנות את קידוד הקובץ, לעבור על הקובץ ככולו או רק חלק ממנו, והרבה עוד.

## ראה גם

- [מדריך לכתיבת קוד קריא ונקי ב TypeScript](https://medium.com/@tetyamin/documentation-coding-with-ts-starter-measures-1737883761ea)
- [הדרכת מרובי טייפים ב TypeScript](https://www.typescriptlang.org/docs/handbook/declaration-files/do-s-and-don-ts.html)
- [מדריך לכתיבת דוקומנטציה טובה עבור פרוייקטי TypeScript](https://blog.bitsrc.io/build-good-documentation-for-your