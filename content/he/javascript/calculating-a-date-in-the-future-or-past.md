---
title:                "חישוב תאריך בעתיד או בעבר"
html_title:           "Javascript: חישוב תאריך בעתיד או בעבר"
simple_title:         "חישוב תאריך בעתיד או בעבר"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## מה זה ולמה?
חישוב תאריך בעתיד או בעבר הוא תהליך של מציאת תאריך מסוים מתוך נתונים נתונים כמו 'היום שאחריי היום' או 'בעוד 2 שנים ו3 חודשים'. התכנתים משתמשים בה בעזרת קוד JavaScript לשימושים כמו לתזמן אירועים, לעקוב אחרי זמני משלוח או לחלק חווית משתמש מותאמת אישית.

## איך עושים את זה:
הנה מקוד JavaScript שמדגים איך לחשב תאריך בעתיד או בעבר:

```Javascript
// חישוב תאריך המחר
let tomorrow = new Date();
tomorrow.setDate(tomorrow.getDate() + 1);
console.log(tomorrow);

// חישוב תאריך מהעבר (למשל, אתמול)
let yesterday = new Date();
yesterday.setDate(yesterday.getDate() - 1);
console.log(yesterday);
```

הפלט מהקוד הזה יהיה התאריך של מחר ואתמול.

## צוללים עמוק יותר:
### היסטוריה:
האובייקט Date ב-JavaScript הוא אובייקט מובנה שמספק את התמיכה בעבודה עם תאריכים וזמנים. מאז ECMAScript 5.1, אובייקט ה-Date הורשע ל- ISO-8601, שהוא פורמט אמין לתאריכים.

### אלטרנטיבות:
עבור חישובים מורכבים יותר עם תאריכים, ספריות מקוד של JavaScript כמו Moment.js, date-fns או Day.js יכול להיות בטוב יותר.

### פרטי מימוש:
ה-addDate() ו-setDate() הם שיטות של האובייקט Date. addDate() מוסיף ימים לתאריך הנוכחי של האובייקט Date, בעוד ש-setDate() מגדיר את התאריך באופן והזמן של האובייקט Date.

## ראה גם:
- [פונקציות תאריך מובנות ב-JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js - ספרייה מקוד JavaScript לעבודה עם תאריכים](https://momentjs.com/)
- [date-fns - המרת ופונקציות נוספות לתאריך](https://date-fns.org/)
- [ECMAScript 5.1 - התקנים לJavaScript](https://www.ecma-international.org/ecma-262/5.1/)