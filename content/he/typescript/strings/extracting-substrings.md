---
date: 2024-01-20 17:46:50.611366-07:00
description: "\u05DC\u05D7\u05DC\u05E5 \u05EA\u05EA-\u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05D5\u05EA \u05E4\u05D9\u05E8\u05D5\u05E9\u05D5 \u05DC\u05E7\u05D7\u05EA \u05D7\
  \u05DC\u05E7 \u05DE\u05EA\u05D5\u05DA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05E7\
  \u05D9\u05D9\u05DE\u05EA. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\
  \u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DC\u05D2\u05D6\u05D5\u05E8 \u05DE\u05D9\
  \u05D3\u05E2 \u05E1\u05E4\u05E6\u05D9\u05E4\u05D9 \u05DE\u05DE\u05D7\u05E8\u05D5\
  \u05D6\u05EA \u05D2\u05D3\u05D5\u05DC\u05D4 \u05D9\u05D5\u05EA\u05E8 - \u05DC\u05D3\
  \u05D5\u05D2\u05DE\u05D4, \u05DC\u05D4\u05D5\u05E6\u05D9\u05D0 \u05E9\u05DD \u05DE\
  \u05E9\u05EA\u05DE\u05E9 \u05DE\u05DB\u05EA\u05D5\u05D1\u05EA \u05DE\u05D9\u05D9\
  \u05DC."
lastmod: '2024-03-13T22:44:38.899268-06:00'
model: gpt-4-1106-preview
summary: "\u05DC\u05D7\u05DC\u05E5 \u05EA\u05EA-\u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\
  \u05EA \u05E4\u05D9\u05E8\u05D5\u05E9\u05D5 \u05DC\u05E7\u05D7\u05EA \u05D7\u05DC\
  \u05E7 \u05DE\u05EA\u05D5\u05DA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05E7\u05D9\
  \u05D9\u05DE\u05EA. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\
  \u05D9\u05DD \u05D6\u05D0\u05EA \u05DC\u05D2\u05D6\u05D5\u05E8 \u05DE\u05D9\u05D3\
  \u05E2 \u05E1\u05E4\u05E6\u05D9\u05E4\u05D9 \u05DE\u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05EA \u05D2\u05D3\u05D5\u05DC\u05D4 \u05D9\u05D5\u05EA\u05E8 - \u05DC\u05D3\u05D5\
  \u05D2\u05DE\u05D4, \u05DC\u05D4\u05D5\u05E6\u05D9\u05D0 \u05E9\u05DD \u05DE\u05E9\
  \u05EA\u05DE\u05E9 \u05DE\u05DB\u05EA\u05D5\u05D1\u05EA \u05DE\u05D9\u05D9\u05DC\
  ."
title: "\u05D7\u05D9\u05DC\u05D5\u05E5 \u05EA\u05EA-\u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05D5\u05EA"
weight: 6
---

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
