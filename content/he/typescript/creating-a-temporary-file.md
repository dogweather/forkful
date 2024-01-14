---
title:    "TypeScript: יצירת קובץ זמני"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/typescript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## למה

יצירת קובץ זמני היא כלי שימושי לכתיבת קוד בטיפול וניהול קבצים. היא מאפשרת לנו ליצור קובץ שימושי בזמן הרצת התוכנית ולאחר מכן למחוק אותו כאשר לא נדרש יותר.

## איך לעשות זאת

כדי ליצור קובץ זמני בטיפוסקריפט, אנו צריכים להשתמש בפונקציה "fs" המובנית של השפה. הנה דוגמא ליצירת קובץ זמני וכיצד להדפיס את התוכן שלו:

```TypeScript
import fs from 'fs';

const tempFile = fs.mkdtempSync('myTempFile_'); // יוצר קובץ זמני עם קידומת myTempFile_
fs.writeFileSync(tempFile, 'התוכן שלי'); // כותב לתוכן לקובץ הזמני שנוצר
console.log(fs.readFileSync(tempFile, 'utf-8')); // מדפיס את התוכן של הקובץ הזמני
// התוכן שלי
```

עם זאת, חשוב לזכור שנהורד לסגור את הקובץ הזמני לאחר שסיימנו להשתמש בו. ניתן לעשות זאת באמצעות פונקציות כמו "fs.unlinkSync" או "fs.unlink".

## מעמקים

יצירת קובץ זמני בטיפוסקריפט כוללת מספר שלבים. ראשית, אנו צריכים ליצור קובץ בעזרת הפונקציה "mkdtempSync" ולהעביר אליה את הקידומת המיוחדת שלנו. לאחר מכן, יש לכתוב תוכן לקובץ בעזרת הפונקציה "writeFileSync" ולקרוא ממנו עם "readFileSync". סוף סוף, יש למחוק את הקובץ באמצעות "unlinkSync".

## ראה גם

- תיעוד מפורט לפונקציות "fs" של טיפוסקריפט: https://nodejs.org/api/fs.html#fs_file_system
- מאמר על יצירת קובץ זמני בטיפוסקריפט: https://dev.to/shahil/stupid-easy-temporary-file-in-nodejs-using-tmp-filthy-easy--2nd5