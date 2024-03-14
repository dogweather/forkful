---
date: 2024-01-20 17:52:21.143378-07:00
description: "\u05D0\u05D9\u05E0\u05D8\u05E8\u05E4\u05D5\u05DC\u05E6\u05D9\u05D9\u05EA\
  \ \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05D4\u05D9\u05D0 \u05E9\u05D9\u05DC\
  \u05D5\u05D1 \u05E9\u05DC \u05D8\u05E7\u05E1\u05D8 \u05D5\u05E0\u05EA\u05D5\u05E0\
  \u05D9\u05DD \u05D1\u05EA\u05D5\u05DA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D0\
  \u05D7\u05EA. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD\
  \ \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05E4\u05E9\u05D8 \u05D4\u05E8\u05DB\
  \u05D1\u05D4 \u05E9\u05DC \u05D8\u05E7\u05E1\u05D8\u05D9\u05DD \u05D3\u05D9\u05E0\
  \u05DE\u05D9\u05D9\u05DD \u05D5\u05DC\u05E2\u05E9\u05D5\u05EA \u05D0\u05EA \u05D4\
  \u05E7\u05D5\u05D3 \u05D9\u05D5\u05EA\u05E8 \u05E7\u05E8\u05D9\u05D0."
lastmod: '2024-03-13T22:44:38.894371-06:00'
model: gpt-4-1106-preview
summary: "\u05D0\u05D9\u05E0\u05D8\u05E8\u05E4\u05D5\u05DC\u05E6\u05D9\u05D9\u05EA\
  \ \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05D4\u05D9\u05D0 \u05E9\u05D9\u05DC\
  \u05D5\u05D1 \u05E9\u05DC \u05D8\u05E7\u05E1\u05D8 \u05D5\u05E0\u05EA\u05D5\u05E0\
  \u05D9\u05DD \u05D1\u05EA\u05D5\u05DA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D0\
  \u05D7\u05EA. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD\
  \ \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05E4\u05E9\u05D8 \u05D4\u05E8\u05DB\
  \u05D1\u05D4 \u05E9\u05DC \u05D8\u05E7\u05E1\u05D8\u05D9\u05DD \u05D3\u05D9\u05E0\
  \u05DE\u05D9\u05D9\u05DD \u05D5\u05DC\u05E2\u05E9\u05D5\u05EA \u05D0\u05EA \u05D4\
  \u05E7\u05D5\u05D3 \u05D9\u05D5\u05EA\u05E8 \u05E7\u05E8\u05D9\u05D0."
title: "\u05E9\u05E8\u05D1\u05D5\u05D1 \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA"
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
