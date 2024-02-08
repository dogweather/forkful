---
title:                "חיפוש והחלפת טקסט"
aliases:
- he/typescript/searching-and-replacing-text.md
date:                  2024-01-20T17:59:07.703476-07:00
model:                 gpt-4-1106-preview
simple_title:         "חיפוש והחלפת טקסט"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/searching-and-replacing-text.md"
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
