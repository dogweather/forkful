---
title:                "בדיקה האם תיקייה קיימת"
date:                  2024-01-20T14:58:50.362143-07:00
html_title:           "Gleam: בדיקה האם תיקייה קיימת"
simple_title:         "בדיקה האם תיקייה קיימת"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## מה ולמה?
בדיקה אם תיקיה קיימת מאפשרת למתכנתים לוודא שהדברים שמוחזרים ושמורים נעשים במקום הנכון בסטורג'. זה חשוב למניעת שגיאות ולוודא תקינות של תהליכים.

## איך לעשות:
ב-Node.js, אפשר להשתמש במודול `fs` כדי לבדוק אם תיקיה קיימת. הדוגמאות מניחות שימוש ב- `fs.promises` עבור async/await.

```TypeScript
import { promises as fsPromises } from 'fs';

async function checkDirectoryExists(path: string): Promise<boolean> {
  try {
    await fsPromises.access(path);
    return true;
  } catch (error) {
    return false;
  }
}

async function main() {
  const directoryPath = './path/to/your/directory';
  const exists = await checkDirectoryExists(directoryPath);
  
  if (exists) {
    console.log('התיקיה קיימת.');
  } else {
    console.log('התיקיה אינה קיימת.');
  }
}

main();
```
פלט לדוגמה:
```
התיקיה קיימת.
```
או לחילופין:
```
התיקיה אינה קיימת.
```

## צלילה עמוקה:
בעבר, היינו משתמשים בפונקציה `fs.existsSync()`, אך היא לא מומלצת לשימוש בעקבות שימוש חוסם. גישה מודרנית יותר היא להשתמש ב- `fs.promises` לקבלת פעולות אסינכרוניות. זה אומר שהקוד שלנו לא יחסום את הלופ של האירועים של Node.js. פונקציית `access` בודקת אם למשתמש יש הרשאה לגשת לקובץ או תיקיה. אם הפונקציה מחזירה שגיאה, זה אומר שהתיקיה לא קיימת או שאין הרשאה.

## ראה גם:
1. דוקומנטציה של Node.js על `fsPromises.access`: https://nodejs.org/api/fs.html#fspromisesaccesspath-mode
2. מדריך על async/await ב-TypeScript: https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-1.html
3. המדריך לפיתוח עם Node.js: https://nodejs.org/en/docs/guides/
