---
date: 2024-01-26 04:15:50.448331-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: Node.js \u05DE\u05E1\
  \u05E4\u05E7 REPL \u05E0\u05D2\u05D9\u05E9\u05D4 \u05D3\u05E8\u05DA \u05D4\u05D8\
  \u05E8\u05DE\u05D9\u05E0\u05DC. \u05E4\u05EA\u05D7\u05D5 \u05D0\u05D5\u05EA\u05D4\
  , \u05D5\u05D0\u05EA\u05DD \u05DE\u05D5\u05DB\u05E0\u05D9\u05DD \u05DC\u05D4\u05EA\
  \u05D7\u05D9\u05DC. \u05D4\u05E0\u05D4 \u05D8\u05E2\u05D9\u05DE\u05D4."
lastmod: '2024-03-13T22:44:39.977349-06:00'
model: gpt-4-0125-preview
summary: "Node.js \u05DE\u05E1\u05E4\u05E7 REPL \u05E0\u05D2\u05D9\u05E9\u05D4 \u05D3\
  \u05E8\u05DA \u05D4\u05D8\u05E8\u05DE\u05D9\u05E0\u05DC."
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05E2\u05D8\u05E4\u05EA \u05D0\
  \u05D9\u05E0\u05D8\u05E8\u05D0\u05E7\u05D8\u05D9\u05D1\u05D9\u05EA (REPL)"
weight: 34
---

## איך לעשות:
Node.js מספק REPL נגישה דרך הטרמינל. פתחו אותה, ואתם מוכנים להתחיל. הנה טעימה:

```javascript
$ node
> let sum = (a, b) => a + b;
undefined
> sum(5, 10);
15
> .exit
```

פשוט, נכון? להגדיר משתנים, פונקציות, או להריץ לולאות. כשסיימתם, `.exit` מחזירה אתכם לעולם האמיתי.

## צלילה עמוקה
REPLs היו קיימות מאז שנות ה-60 – LISP פיתחה את הקונספט. הרעיון: לתת מידע מיידי למתכנת. אופציות אחרות? למעט REPL של Node.js, יש גם קונסולות מבוססות דפדפן כמו Chrome DevTools, אזורי חול וירטואליים באינטרנט כמו JSFiddle, או סביבות פיתוח משולבות (IDEs) מלאות כמו VSCode עם מגרשי משחק אינטראקטיביים.

מאחורי הקלעים, זרימת העבודה של REPL בדרך כלל:
1. קריאת קלט
2. קומפילציה וביצוע קוד
3. הדפסת פלט
4. חזרה לשלב התחילה

זהו מחזור פשוט וכל כך יעיל שהשפיע באופן משמעותי על התכנות האינטראקטיבי.

## ראו גם
- [תיעוד REPL של Node.js](https://nodejs.org/api/repl.html)
- [הקדמה של מוזילה למודולים של JavaScript ב-REPLs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Modules)
- [JSFiddle](https://jsfiddle.net/)
