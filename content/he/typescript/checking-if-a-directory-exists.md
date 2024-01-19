---
title:                "בדיקה אם ספרייה קיימת"
html_title:           "Java: בדיקה אם ספרייה קיימת"
simple_title:         "בדיקה אם ספרייה קיימת"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## מה ולמה? 

בדיקה אם ספרייה קיימת היא בדיקה שמחזירה תוצאה בוליאנית כאשר נשאל אודות הקיום של ספרייה במערכת הקבצים. תכנתים מבצעים את זה לשם אימות התקנה, בדיקת תקפות נתיב קבצים ועוד.

## איך לעשות: 

דוגמאות קידומת ופלט דוגמא באמצעות מסגרת ```TypeScript ... ```.

```TypeScript
import fs from 'fs';

function isDirectoryExists(path: string): boolean {
  return fs.existsSync(path) && fs.lstatSync(path).isDirectory();
}

console.log(isDirectoryExists('/path/to/directory'));  // תחזיר true או false
```

## צלילה עמוקה

(1) במקרה שלנו, הפעולה fs.existsSync היא פעולה שנוספה לNode.js מאז הגרסה v0.1.21. היא מספקת בדיקה סינכרונית של הקיום של נתיב או ספרייה.
(2) כמו כך, ישנן אפשרויות נוספות למימוש, כולל החיפוש אחר שגיאות כאשר מנסים לגשת אל הספרייה (המסופרת במקום השני ממשמע שהקיום לא הוכח) או שימוש בפקודת shell לביצוע הבדיקה.
(3) מימוש הדוגמה שהוצגה כאן משתמש בfs.existsSync בשילוב עם fs.lstatSync(path).isDirectory() כדי לאמת אם הנתיב מצביע לא לקובץ קיים אלא לספרייה.

## ראה גם

- מסמך התיעוד של Node.js על פעולות FileSystem: https://nodejs.org/api/fs.html
- מאמר על בדיקת קיומם של קבצים וספריות ב-Node.js: https://stackabuse.com/how-to-check-if-a-file-or-directory-exists-in-node-js/