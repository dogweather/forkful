---
title:                "שימוש במעטפת אינטראקטיבית (REPL)"
aliases: - /he/javascript/using-an-interactive-shell-repl.md
date:                  2024-01-26T04:15:50.448331-07:00
model:                 gpt-4-0125-preview
simple_title:         "שימוש במעטפת אינטראקטיבית (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## מה ולמה?
מעטפות אינטראקטיביות, או REPLs (לולאות קריאה-חישוב-הדפסה), מאפשרות להריץ קוד באופן מיידי, לבדוק פונקציות, אלגוריתמים, או לשחק עם רעיונות. הן מהוות את דפי הטיוטה של התכנות, מהירות ולא מסודרות, בלי להקים סביבת פיתוח מלאה.

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
