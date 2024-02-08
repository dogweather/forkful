---
title:                "חילוץ תת-מחרוזות"
aliases:
- he/typescript/extracting-substrings.md
date:                  2024-01-20T17:46:50.611366-07:00
model:                 gpt-4-1106-preview
simple_title:         "חילוץ תת-מחרוזות"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## מה ולמה?
לחלץ תת-מחרוזות פירושו לקחת חלק מתוך מחרוזת קיימת. מתכנתים עושים זאת לגזור מידע ספציפי ממחרוזת גדולה יותר - לדוגמה, להוציא שם משתמש מכתובת מייל.

## איך לעשות:
TypeScript מציע מספר דרכים לחלץ תת-מחרוזות. הנה דוגמאות עם פלט הדוגמא:

```TypeScript
// שימוש בפונקציה substring()
let email: string = "yossi@example.com";
let username = email.substring(0, email.indexOf('@'));
console.log(username); // פלט: yossi

// שימוש בפונקציה slice()
let emailSlice: string = "dani@example.com";
let domain = emailSlice.slice(emailSlice.indexOf('@') + 1);
console.log(domain); // פלט: example.com
```

## מבט עמוק:
חילוץ תת-מחרוזות הוא מקרה שימוש נפוץ בתכנות מאז התחלות הקומפיוטרים. בימים שבהם הזיכרון היה יקר, חשוב היה להבין בדיוק איך ומתי לבצע פעולה זו. ב-TypeScript, יש לנו את הפונקציה `substring()` שמחזירה חלק ממחרוזת, ו`slice()` שעובדת בצורה דומה אך יכולה לקבל גם אינדקס סיום שלילי. עוד אופציה היא `substr()` אבל היא נחשבת לדפריקטד, ולכן עדיף להימנע משימוש בה. כשאנחנו מחלצים תת-מחרוזת, חשוב לזכור שב-TypeScript הספירה מתחילה מאינדקס 0.

## ראו גם:
- [MDN Web Docs - String.prototype.substring()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [MDN Web Docs - String.prototype.slice()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- [TypeScript Handbook: Basic Types - String](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
