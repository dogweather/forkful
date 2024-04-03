---
date: 2024-01-20 17:59:02.304714-07:00
description: "\u05D7\u05D9\u05E4\u05D5\u05E9 \u05D5\u05D4\u05D7\u05DC\u05E4\u05EA\
  \ \u05D8\u05E7\u05E1\u05D8 \u05D4\u05DD \u05DB\u05DC\u05D9\u05DD \u05D1\u05E1\u05D9\
  \u05E1\u05D9\u05D9\u05DD \u05D1\u05EA\u05DB\u05E0\u05D5\u05EA - \u05D0\u05E0\u05D7\
  \u05E0\u05D5 \u05DE\u05D5\u05E6\u05D0\u05D9\u05DD \u05D8\u05E7\u05E1\u05D8 \u05DE\
  \u05E1\u05D5\u05D9\u05DD \u05D5\u05DE\u05D7\u05DC\u05D9\u05E4\u05D9\u05DD \u05D0\
  \u05D5\u05EA\u05D5 \u05D1\u05D8\u05E7\u05E1\u05D8 \u05D0\u05D7\u05E8. \u05EA\u05DB\
  \u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA\
  \ \u05D6\u05D4 \u05DC\u05E9\u05D9\u05E4\u05D5\u05E8 \u05E7\u05D5\u05D3, \u05E0\u05D9\
  \u05E7\u05D5\u05D9 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05D0\u05D5 \u05DC\u05E9\
  \u05D9\u05E0\u05D5\u05D9\u05D9\u05DD\u2026"
lastmod: '2024-03-13T22:44:39.950658-06:00'
model: gpt-4-1106-preview
summary: "\u05D7\u05D9\u05E4\u05D5\u05E9 \u05D5\u05D4\u05D7\u05DC\u05E4\u05EA \u05D8\
  \u05E7\u05E1\u05D8 \u05D4\u05DD \u05DB\u05DC\u05D9\u05DD \u05D1\u05E1\u05D9\u05E1\
  \u05D9\u05D9\u05DD \u05D1\u05EA\u05DB\u05E0\u05D5\u05EA - \u05D0\u05E0\u05D7\u05E0\
  \u05D5 \u05DE\u05D5\u05E6\u05D0\u05D9\u05DD \u05D8\u05E7\u05E1\u05D8 \u05DE\u05E1\
  \u05D5\u05D9\u05DD \u05D5\u05DE\u05D7\u05DC\u05D9\u05E4\u05D9\u05DD \u05D0\u05D5\
  \u05EA\u05D5 \u05D1\u05D8\u05E7\u05E1\u05D8 \u05D0\u05D7\u05E8."
title: "\u05D7\u05D9\u05E4\u05D5\u05E9 \u05D5\u05D4\u05D7\u05DC\u05E4\u05EA \u05D8\
  \u05E7\u05E1\u05D8"
weight: 10
---

## מה ולמה?
חיפוש והחלפת טקסט הם כלים בסיסיים בתכנות - אנחנו מוצאים טקסט מסוים ומחליפים אותו בטקסט אחר. תכניתנים עושים את זה לשיפור קוד, ניקוי נתונים, או לשינויים מהירים ויעילים.

## איך לעשות:
כדי לבצע חיפוש והחלפה ב-JavaScript אנחנו משתמשים במתודה `.replace()` של מחרוזות. קחו נניח את הדוגמא הבאה:

```javascript
let text = "בוקר טוב, ישראל! בוקר טוב ומלא שמש!";
let newText = text.replace("בוקר טוב", "ערב טוב");
console.log(newText); // ערב טוב, ישראל! ערב טוב ומלא שמש!
```

אם אנו רוצים להחליף את כל המופעים ולא רק את הראשון, נשתמש בביטוי רגולרי עם הדגל `g` (global):

```javascript
let text = "בוקר טוב, ישראל! בוקר טוב ומלא שמש!";
let newText = text.replace(/בוקר טוב/g, "ערב טוב");
console.log(newText); // ערב טוב, ישראל! ערב טוב ומלא שמש!
```

## טבילה עמוקה:
מאז ימי JavaScript הראשונים, יכולת החיפוש והחלפה הייתה חלק אינטגרלי במניפולציית מחרוזות. עם זאת, גישות יכולות להשתנות. אחת להצטיין היא השימוש בביטויים רגולריים, המאפשרים חיפוש מתקדם וגמיש. לדוגמא, החלפת כל האותיות הקטנות בטקסט: 

```javascript
let text = "שלום לכולם! ברוכים הבאים.";
let newText = text.replace(/[א-ת]/g, (match) => match.toUpperCase());
console.log(newText); // שלום לכולם! ברוכים הבאים.
```

תחת `match` נכנסת כל מחרוזת תווים שהמצאנו, והפונקציה מחזירה אותה לאחר שעשינו איתה משהו - כאן, שינוי לאותיות גדולות.

## לראות גם:
- [MDN Web Docs: String.prototype.replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [Regular Expressions MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [JavaScript Info: String](https://javascript.info/string)

קריאה מועילה בהמשך לפרטים נוספים, דוגמאות והסברים על מניפולציות מחרוזות ושימוש בביטויים רגולריים ב-JavaScript.
