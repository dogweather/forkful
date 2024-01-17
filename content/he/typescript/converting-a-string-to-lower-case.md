---
title:                "המרת מחרוזת לאותיות קטנות"
html_title:           "TypeScript: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## מה & למה?
המרת מחרוזת לאותיות קטנות היא פעולה שמשתמשים בה בתכנות בכדי לשנות את האותיות של מחרוזת לאותיות קטנות. פעולה זו קלה לביצוע וככל שמחשבים מתקדמים, היא נעשית בצורה יותר אוטומטית ומהירה. תוכניות תמיד תעשה צריכה במקרים שונים, כגון השוואת מחרוזות ללא התחשבות באותיות גדולות או קטנות.

## איך לעשות:
```TypeScript
let str = "HELLO FROM TYPESCRIPT!";
console.log(str.toLowerCase());
// output: hello from typescript!
```

```TypeScript
let str = "HeLlO fRoM tYpEsCrIpT!";
console.log(str.toLowerCase());
// output: hello from typescript!
```

## מעמקים:
בעבר, פעולת המרה לאותיות קטנות הייתה יותר מורכבת ודרשה שימוש בפונקציות ומשתנים נוספים. כיום, היא מהווה חלק בלתי נפרד מהתכנים ומהווה מתודולוגיה בסיסית שכל המפתחים יכולים להשתמש בה בקלות. ישנן פעולות אחרות של המרה לאותיות קטנות כגון פעולת המרה לאותיות גדולות או מעבר לטקסט הפוך בין אותיות גדולות לקטנות.

## ראו גם:
למידע נוסף ודוגמאות בקוד לפעולת המרה לאותיות קטנות, אנו מציעים לעיין בפנקס הטכנולוגי של TypeScript המכיל מידע מקיף על תכונות ומתודות נוספות. ניתן למצוא קישורים לפנקס בדף הרשמי של TypeScript או בספריות המתאימות.