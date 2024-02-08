---
title:                "חיפוש והחלפת טקסט"
aliases:
- he/javascript/searching-and-replacing-text.md
date:                  2024-01-20T17:59:02.304714-07:00
model:                 gpt-4-1106-preview
simple_title:         "חיפוש והחלפת טקסט"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

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
