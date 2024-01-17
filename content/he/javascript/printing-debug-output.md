---
title:                "הדפסת פלט מנקודת נחישות"
html_title:           "Javascript: הדפסת פלט מנקודת נחישות"
simple_title:         "הדפסת פלט מנקודת נחישות"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

## מה ולמה?

הדפסת פלט דיבאג (debug output) היא כלי ביצוע בקוד שמאפשר למפתחים לראות מידע נוסף על הרצת התוכנית שלהם. זה נעשה על מנת לזהות בעיות ולתקן אותן. מפתחים משתמשים בדיבאג פלט כדי לתקן באגים, לשפר ביצועים ולהבין מה קורה בתוך הקוד שלהם.

## איך לעשות זאת:

השתמשו בקוד להדפסת דיבאג עם ערכים משתנים כדי לראות מידע נוסף בזמן הריצה של התוכנית:

```Javascript
console.log("הפלט שלי הוא:", myOutput);
```

תוצאה של הקוד הנ"ל יהיה:

```
הפלט שלי הוא: Hello World!
```

## חקירה מתחומקת:

- **הקשר ההיסטורי:** בעבר, מפתחים היו משתמשים בדפדוף כדי לזהות באגים ובעיות. אך הדפדפן אינו מושלם ולעיתים ישנם באגים שאינם מוצגים בעת ההדפסה. לכן, הדפסת פלט דיבאג הפך לכלי חשוב יותר בשנים האחרונות.

- **אלטרנטיבות:** דבר נוסף שניתן לעשות במקום להדפיס דיבאג פלט הוא להשתמש בכלי לדיבאג כמו `debugger` בכדי להשעית את הקוד ולבדוק נתונים בתוך הקוד.

- **פירוט טכני:** בjavascript, ישנם כמה שיטות להדפסת דיבאג פלט: יש אפשרות להשתמש בפונקציה `console.log()` או להשתמש בערך `debugger` ישירות. כדי להשתמש בכלי debug בדפדוף, עליכם לפתוח נתיב לדף עם כלי הdebug כבעלי הדפדפן שלכם.

## ראו גם:

- [כלי הדיבאג בג'אווהסקריפט](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/debugger)
- [הדפסה לדוגמא בפייתון](https://code.visualstudio.com/docs/editor/debugging#_print-statements)
- [מדריך להשתמש בכלי דיבאג ביעילות](https://www.sitepoint.com/javascript-debugging-tips-and-tricks/)