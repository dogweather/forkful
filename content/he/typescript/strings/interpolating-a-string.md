---
title:                "שרבוב מחרוזת"
aliases:
- /he/typescript/interpolating-a-string/
date:                  2024-01-20T17:52:21.143378-07:00
model:                 gpt-4-1106-preview
simple_title:         "שרבוב מחרוזת"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/interpolating-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
אינטרפולציית מחרוזות היא שילוב של טקסט ונתונים בתוך מחרוזת אחת. תכנתים עושים זאת כדי לפשט הרכבה של טקסטים דינמיים ולעשות את הקוד יותר קריא.

## איך לעשות:
להלן דוגמאות קוד המדגימות איך לבצע אינטרפולציית מחרוזות ב-TypeScript.

```TypeScript
// משתנים לדוגמא
const user = 'נעמה';
const balance = 1023.45;

// אינטרפולציית מחרוזות עם Template Strings
const greeting = `שלום ${user}, יתרתך היא ${balance} ש"ח.`;
console.log(greeting);  // "שלום נעמה, יתרתך היא 1023.45 ש"ח."
```

## עיון מעמיק:
הרעיון של אינטרפולציית מחרוזות אינו חידוש במיוחד – הוא קיים בשפות תכנות אחרות מזמן. ב-JavaScript (ובהתאם ב-TypeScript), היישום הזה הפך להיות פופולרי ביותר עם הגעת תקן ES6, שהכניס את המושג של Template Strings או Template Literals. קוד לפני ES6 היה נתמך על חיבור מחרוזות באמצעות האופרטור `+`. זו אפשרות שעדיין קיימת אבל פחות נוחה.

אבל לא רק לנוחות השימוש יש כאן חשיבות, אלא גם לביצועים. אינטרפולציות מחרוזת מבוצעות בזמן ריצה ויכולות להיות פחות יעילות מאשר שימוש במחרוזות קבועות. אך עם זאת, מנועי JavaScript מודרניים משפרים באופן קבוע את התיעול של אופרציות אלה, ולכן ההבדל בביצועים נעשה פחות ופחות משמעותי.

## ראה גם:
- תיעול ביצועים של Template Strings: [Optimizing JavaScript Performance](https://v8.dev/blog/cost-of-javascript-2019)
- השוואה בין אינטרפולציית מחרוזות לחיבור רגיל של מחרוזות: [String concatenation vs. Template Literals](https://developers.google.com/web/updates/2015/01/ES6-Template-Strings)
