---
title:                "בדיקה אם קיימת תיקייה"
html_title:           "TypeScript: בדיקה אם קיימת תיקייה"
simple_title:         "בדיקה אם קיימת תיקייה"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## למה
ישנם רבים שמתחילים ללמוד TypeScript ונתקלים בשגיאה של "התיקייה אינה קיימת". לאחר מכן הם מתחילים לחפש דרך למצוא אם התיקייה קיימת בפונקציה המתאימה. במאמר זה תלמדו כיצד לבדוק אם תיקייה קיימת ב TypeScript לפתור את בעיות השגיאה הנ"ל.

## איך לעשות זאת
הנה כמה דוגמאות קוד עם פלט בתוך קטעי קוד "```TypeScript ... ```".

כדי לבדוק אם תיקייה קיימת בצורה פשוטה, ניתן להשתמש בפונקציה המובנית "fs.existsSync" שמקבלת את הנתיב של התיקייה כפרמטר. אם התיקייה קיימת, הפונקציה תחזיר אמת (true), אחרת תחזיר שקר (false).

```TypeScript
import * as fs from 'fs';

const path = './myFolder';
const directoryExists = fs.existsSync(path);

console.log(directoryExists); // יוצא: true אם התיקייה קיימת, אחרת יוצא: false
```

למרבה המזל, יש פונקציה נוספת נקראת "fs.statSync" שמקבלת את הנתיב של התיקייה כפרמטר ומחזירה אובייקט במידע על התיקייה, כולל האם היא קיימת או לא.

```TypeScript
import * as fs from 'fs';

const path = './myFolder';
const directoryStats = fs.statSync(path);

if (directoryStats.isDirectory()) {
  console.log('התיקייה קיימת'); // יוצא: התיקייה קיימת אם התיקייה קיימת
} else {
  console.log('התיקייה לא קיימת'); // יוצא: התיקייה לא קיימת אם התיקייה לא קיימת
}
```

## Deep Dive
הפונקציות "fs.existsSync" ו"fs.statSync" משתמשות במנגנוני קריאת קבצים המובנים במערכת ההפעלה כדי לבדוק אם התיקייה קיימת. אם תרצו לקרוא את התיקייה ולקבלת מידע מעבר לזה, ניתן להשתמש בפונקציה "fs.readdirSync" שתחזיר את התוכן של התיקייה.

כדי לתמוך