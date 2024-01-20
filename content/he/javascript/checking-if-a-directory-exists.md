---
title:                "בדיקה האם תיקייה קיימת"
date:                  2024-01-20T14:57:34.512034-07:00
html_title:           "Gleam: בדיקה האם תיקייה קיימת"
simple_title:         "בדיקה האם תיקייה קיימת"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## מה ולמה?
בדיקה אם תיקייה קיימת ב-JavaScript היא עניין של קריאה למערכת הפעלה כדי לזהות אם נתיב מסוים הוא באמת תיקייה. פרוגרמרים עושים זאת לפני פעולות כמו קריאה, כתיבה או יצירת קבצים כדי למנוע שגיאות.

## איך לעשות:
כדי לבדוק אם תיקייה קיימת, אתה יכול להשתמש במודול `fs` של Node.js, עם הפונקציה `fs.existsSync()` או באופן אסינכרוני עם `fs.promises.stat()`:

```Javascript
const fs = require('fs');

// בדיקה סינכרונית
const directoryExistsSync = fs.existsSync('/path/to/directory');
console.log(directoryExistsSync); // ידפיס true או false

// בדיקה אסינכרונית
const checkDirectoryExists = async (path) => {
  try {
    const stat = await fs.promises.stat(path);
    return stat.isDirectory();
  } catch (error) {
    if (error.code === 'ENOENT') { // תיקייה לא נמצאה
      return false;
    }
    throw error; // שגיאה אחרת
  }
};

checkDirectoryExists('/path/to/directory').then(console.log); // ידפיס true או false
```

## צלילה עמוקה:
בגרסאות ישנות יותר של Node.js, היו רק אפשרויות סינכרוניות כמו `fs.existsSync()`. היום יש נטייה להשתמש בפונקציות אסינכרוניות כדי לא לחסום את הלולאה הראשית. אפשר לעשות את זה עם `fs.promises.stat()` או עם `fs.promises.access()` ולבדוק את הפרמטר `fs.constants.F_OK`. זהירות עם שגיאות בשימוש במודול fs, שכן הן יכולות להשפיע על זרימת הקוד שלך.

## ראה גם:
- דוקומנטציה של Node.js למודול `fs`: https://nodejs.org/api/fs.html
- מדריך לעבודה עם מערכת הקבצים ב-Node.js: https://nodejs.dev/learn/the-nodejs-fs-module
- דוקומנטציה של JavaScript promises: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Using_promises