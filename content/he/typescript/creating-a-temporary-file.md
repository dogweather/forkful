---
title:                "יצירת קובץ זמני"
html_title:           "TypeScript: יצירת קובץ זמני"
simple_title:         "יצירת קובץ זמני"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Why
למה יש צורך ליצור קובץ זמני בתוכנית טייפסקריפט? יתכן שישנם מספר סיבות, אחת מהן יכולה להיות לצורך בדיקה של קוד או ליצירת פתרון זמני כאשר יש צורך בפעולה מהירה וזמנית בלבד.

## How To
 כדי ליצור קובץ זמני בטייפסקריפט, ניתן להשתמש בפונקציה המובנית "tmpfile()" של שפת התכנות. פונקציה זו מחזירה מצביע לקובץ זמני חדש שנוצר וניתן לבצע עליו פעולות כרצוננו. ניתן לראות בקוד המצורף מקרה של שימוש בפונקציה זו ליצירת קובץ זמני וכתיבה לתוכו:
```typescript
let tempFile = tmpfile();
fs.writeSync(tempFile, "Temporary file created through TypeScript!");
console.log("Temporary file created and written to successfully.");
```
בתעשיית התוכנה ישנם גם ספריות חיצוניות שניתן להשתמש בהן ליצירת קבצים זמניים. כדי להשתמש בספריית כזו, ניתן להתקין אותה באמצעות פקודת ההתקנה המתאימה כמו npm ולאחר מכן לקרוא לפונקציות הנדרשות ליצירת קבצים זמניים. לדוגמה, הספרייה "tmp" מספקת פונקציות מתקדמות לניהול ויצירת קבצים זמניים ואפשרויות נוספות:
```typescript
//installing tmp library using npm
npm install tmp

//creating temporary file with tmp library
tmp.file(function _tempFileCreated(err, path, fd, cleanupCallback) {
  if (err) throw err;
  console.log("Temporary file created at " + path);
  fs.writeSync(fd, "Temporary file created through tmp library!");
  console.log("Temporary file written to successfully.");
});
```

## Deep Dive
המטרה הראשונית של תכנות קוד היא ליצור פתרונות יעילים ויעילים עבור הבעיות השונות שעלולות להתעורר במשך פיתוח התוכנה. לכן, בכדי לספק פתרון זמני ופורטב