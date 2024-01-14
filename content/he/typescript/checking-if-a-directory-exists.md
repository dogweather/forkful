---
title:                "TypeScript: לבדיקת קיום תיקייה"
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## למה

לבדוק אם תיקייה קיימת במחשב שלך יכול להיות חשוב במצבים שונים בתכנות. לדוגמה, אם אתה עובד עם קבצים או מחלקת מידע ואתה רוצה לוודא שהתיקייה שבה הם נמצאים קיימת לפני שאתה ממשיך עם התהליך האחרון שלך. זה עשוי למנוע מך להתחיל תהליכים שלא יעבדו או ליצור את התיקייה אם לא נמצאת.

## כיצד לעשות זאת 

הנה דוגמא קטנה של פונקציה שתבדוק אם תיקייה קיימת באמצעות TypeScript:

```typescript
const fs = require('fs');

function checkDirectory(path: string): boolean {
    return fs.existsSync(path);
}

console.log(checkDirectory('/Users/Desktop/Data')); // Output: true
console.log(checkDirectory('/Users/Desktop/Nonexistent')); // Output: false
```

כאן אנו משתמשים בפונקציה `existsSync` של החבילה `fs` של Node.js כדי לבדוק אם תיקייה קיימת. הפונקציה מחזירה boolean שמציין האם התיקייה קיימת או לא. אנו מנסים לבדוק את תיקיית `Data` ותיקיית `Nonexistent` בהתאמה ומקבלים את הפלט הרצוי.

## מערכת נשיאה

כדי לעבוד עם תיקיות בכיתוב צוקלצורה היטב אתה צריך להבין כיצד המערכת הפעילה מתייחסת לתיקיות. כאשר אתה מבקש מאת המערכת לבדוק אם תיקייה קיימת, היא תבדוק את התיקייה הנתונה ואת מיקומה בהתאם לנתיב הנתון. אם התיקייה נמצאת במיקום מסוים במחשב שלך, היא תחזיר `true` ואם לא, היא תחזיר `false`.

בנוסף, כדי לבדוק תיקייה מסוימת או אפילו קבצים במיקום מסוים, עליך להיות זכאי לגישה לאותן תיקיות ולקבצים.

## ראה גם

- [Node.js fs documentation](https://nodejs.org/api/fs.html#fs_fs_existssync_path)
- [TypeScript documentation](https://www.typescriptlang