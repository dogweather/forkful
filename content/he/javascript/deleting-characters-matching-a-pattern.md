---
title:                "Javascript: מחיקת תווים התואמים לתבנית"
simple_title:         "מחיקת תווים התואמים לתבנית"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# למה

ניתן למחוק תווים שתואמים את התבנית כדי לקבל טקסט מסודר ולפתור בעיות של ביצועים עם מחרוזות גדולות.

## איך לעשות זאת

שימוש בפונקציית התיקון המובנת string.replace() בוואנילה ג'אווהסקריפט:

```Javascript
let str = "זהו טקסט לדוגמה עם תווים שאנו רוצים למחוק!";
let newStr = str.replace(/[א-ת]/g, ""); // תווים אלפאביתיים בעברית נמחקים
console.log(newStr); // טקסטדוגמעמחוק
```

פרטים נוספים על שימוש בתבניות ומתקן השבת העמוק:

```Javascript
let str = "המחרוזת שאנו רוצים לעבוד איתה.";
let newStr = str.replace(/תבנית/, "מחרוזת חדשה"); // להחלפת המחרוזת המגוונת שאתה רואה בהתאם לתבנית
console.log(newStr); // המחרוזת שאנו רוצים לעבוד איתה.
```

## עמוק יותר

ישנם מספר דרכים שונות למחוק תווים במחרוזת, וכל אחת מהן מתאימה למצבים שונים. ניתן להשתמש בתבניות שמשתמשות בתווים של אלפאנומרי או בתווים נגרי. ניתן גם לשנות את התבנית המקורית, או לשנותה תוך כדי או לאחר הפעולה.

# ראו גם

- [תיעוד JavaScript MDN](https://developer.mozilla.org/he/docs/Web/JavaScript/Guide/Regular_Expressions)
- [RegExp.prototype.test() MDN תיעוד](https://developer.mozilla.org/he/docs/Web/JavaScript/Reference/Global_Objects/RegExp/test)
- [תבניות רגולריות למתחילים - תיעוד אקספרס](https://expressjs.com/en/guide/using-middleware.html#middleware.built-in)