---
date: 2024-01-20 17:59:07.703476-07:00
description: "\u05D7\u05D9\u05E4\u05D5\u05E9 \u05D5\u05D4\u05D7\u05DC\u05E4\u05EA\
  \ \u05D8\u05E7\u05E1\u05D8 \u05D4\u05DD \u05E4\u05E2\u05D5\u05DC\u05D5\u05EA \u05E0\
  \u05E4\u05D5\u05E6\u05D5\u05EA \u05E9\u05DE\u05D0\u05E4\u05E9\u05E8\u05D5\u05EA\
  \ \u05DC\u05DA \u05DC\u05D0\u05EA\u05E8 \u05D7\u05DC\u05E7\u05D9 \u05D8\u05E7\u05E1\
  \u05D8 \u05D1\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D5\u05DC\u05E9\u05E0\u05D5\
  \u05EA \u05D0\u05D5\u05EA\u05DD. \u05EA\u05D5\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\
  \u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DC\u05E9\u05DD \u05EA\
  \u05D7\u05D6\u05D5\u05E7\u05D4, \u05E0\u05D9\u05E7\u05D5\u05D9 \u05E0\u05EA\u05D5\
  \u05E0\u05D9\u05DD \u05D0\u05D5 \u05EA\u05D9\u05E7\u05D5\u05E0\u05D9\u05DD \u05D0\
  \u05D5\u05D8\u05D5\u05DE\u05D8\u05D9\u05D9\u05DD."
lastmod: '2024-03-13T22:44:38.892879-06:00'
model: gpt-4-1106-preview
summary: "\u05D7\u05D9\u05E4\u05D5\u05E9 \u05D5\u05D4\u05D7\u05DC\u05E4\u05EA \u05D8\
  \u05E7\u05E1\u05D8 \u05D4\u05DD \u05E4\u05E2\u05D5\u05DC\u05D5\u05EA \u05E0\u05E4\
  \u05D5\u05E6\u05D5\u05EA \u05E9\u05DE\u05D0\u05E4\u05E9\u05E8\u05D5\u05EA \u05DC\
  \u05DA \u05DC\u05D0\u05EA\u05E8 \u05D7\u05DC\u05E7\u05D9 \u05D8\u05E7\u05E1\u05D8\
  \ \u05D1\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D5\u05DC\u05E9\u05E0\u05D5\u05EA\
  \ \u05D0\u05D5\u05EA\u05DD. \u05EA\u05D5\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD\
  \ \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DC\u05E9\u05DD \u05EA\u05D7\
  \u05D6\u05D5\u05E7\u05D4, \u05E0\u05D9\u05E7\u05D5\u05D9 \u05E0\u05EA\u05D5\u05E0\
  \u05D9\u05DD \u05D0\u05D5 \u05EA\u05D9\u05E7\u05D5\u05E0\u05D9\u05DD \u05D0\u05D5\
  \u05D8\u05D5\u05DE\u05D8\u05D9\u05D9\u05DD."
title: "\u05D7\u05D9\u05E4\u05D5\u05E9 \u05D5\u05D4\u05D7\u05DC\u05E4\u05EA \u05D8\
  \u05E7\u05E1\u05D8"
---

{{< edit_this_page >}}

## מה ולמה?
חיפוש והחלפת טקסט הם פעולות נפוצות שמאפשרות לך לאתר חלקי טקסט במחרוזת ולשנות אותם. תוכניתנים עושים זאת לשם תחזוקה, ניקוי נתונים או תיקונים אוטומטיים.

## איך לעשות:
```typescript
let greeting: string = 'שלום, עולם! שלום לכולם!';
let searchFor: string = 'שלום';
let replaceWith: string = 'להתראות';
let result: string = greeting.replace(new RegExp(searchFor, 'g'), replaceWith);

console.log(result);  // Outputs: להתראות, עולם! להתראות לכולם!
```

בדוגמא מעלה, השתמשנו ב-RegExp כדי להחליף את כל המופעים של "שלום" ב-"להתראות".

## עיון מעמיק:
חיפוש והחלפת טקסט התפתחו ככלי חשובים בעיבוד טקסטים ותכנות מהימים הראשונים של המחשבים. חלופות כוללות שימוש בפונקציות מובנות של מסדי נתונים או שימוש בספריות צד שלישי. ב-TypeScript, החלפת טקסט מבוצעת בדרך כלל על ידי המתודה `.replace()` של מחרוזת, עם פרמטרים לחיפוש או ביטוי רגולרי ולמחרוזת ההחלפה.

## ראו גם:
- [MDN Web Docs on replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [Regular Expressions (MDN)](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [TypeScript Handbook](https://www.typescriptlang.org/docs/handbook/intro.html)
