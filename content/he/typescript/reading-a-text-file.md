---
title:                "TypeScript: קריאת קובץ טקסט"
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## למה

קריאת קובץ טקסט היא כלי חיוני בכתיבת קוד בטיפוסקריפט ומתאים למגוון מטרות. קריאת קובץ טקסט יכולה לסייע בטעינת נתונים, שמירת תוצאות ועוד. קריאת קובץ טקסט היא גם יעילה יותר מאתר קבצים באמצעות אובייקטים JavaScript בגלל היכולת ליישם קוד לפעול על הנתונים בצורה מותאמת אישית.

## איך לבצע קריאת קובץ טקסט

```TypeScript
// קריאת קובץ טקסט עם קוד בסגנון Node.js באמצעות ספריית fs
import * as fs from 'fs';

// קריאת קובץ והמרת תוכן למחרוזת
const data = fs.readFileSync('file.txt', 'utf-8');

// הדפסת התוכן שנקרא
console.log(data);

// דוגמה נוספת עם שימוש בפרמטרי קבצים לתיקיה שלנו
const files = fs.readdirSync("path/to/directory", { withFileTypes: true });
// לולאה על הפרמטרים לאיתור קבצים
for (const file of files) {
    // בדיקה שהפרמטר הוא קובץ ולא תיקיה
    if (!file.isDirectory()) {
        // הדפסת שם הקובץ
        console.log(file.name);
    }
}
```

הפלט של הקוד לעיל יכול להיראות כך:
```
שלום! זהו קובץ טקסט פשוט.
קובץ_אחר.אנד
קובץ_טקסט.txt
```

אם נשתמש בדוגמה השנייה של קריאת קבצים, הפלט יראה בדיוק כמו מחרוזת התיקיה עם שמות הקבצים שנמצאו.

## חקירה מעמיקה

טיפוסקריפט מספק מספר אפשרויות לקריאת קבצים אחרות מלבד ספרייה fs שהוצגה לעיל. למשל, ניתן לקרוא קבצים באמצעות שימוש ב- XMLHttpRequest או באמצעות fetch API. כמו כן, ניתן להשתמש בפונקצי