---
title:                "קבלת התאריך הנוכחי"
date:                  2024-01-20T15:15:10.305683-07:00
simple_title:         "קבלת התאריך הנוכחי"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## מה ולמה?
השגת התאריך הנוכחי ב-JavaScript מתבצעת על ידי יצירת אינסטנס חדש של האובייקט `Date`. תכנתים עושים זאת למגוון סיבות: תיעוד זמנים של פעולות, הצגת תאריכים ושעות למשתמשים, או חישובים המבוססים על זמן.

## איך לעשות:
```Javascript
// יצירת אובייקט תאריך חדש לקבלת התאריך והשעה הנוכחיים
let currentDate = new Date();

// הדפסת התאריך והשעה הנוכחיים לקונסול
console.log(currentDate);

// דוגמא לפלט:
// Wed Apr 05 2023 16:20:35 GMT+0300 (Israel Standard Time)
```

## עיון מעמיק:
האובייקט `Date` ב-JavaScript שימש מאז התקנת השפה כחלק מה-ECMAScript Standard. ישנן מטודות שונות למניפולציה ולאיחזור מידע מן האובייקט הזה, כגון `.getFullYear()`, `.getMonth()`, ועוד. אלטרנטיבות לשימוש באובייקט `Date` הן ספריות חיצוניות כמו Moment.js או date-fns, המציעות ממשקים נוחים ופונקציונליות רחבה יותר. ברוב המקרים, למרות זאת, האובייקט `Date` של JavaScript מספק את הפונקציונליות הנדרשת ללא צורך להתקין תוספים.

## ראה גם:
- [MDN Web Docs - Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date) - מקור מידע אודות האובייקט Date ב-JavaScript.
- [JavaScript Date and Time Functions](https://www.w3schools.com/js/js_dates.asp) - חלק באתר w3schools המראה דוגמאות ומידע נוסף.
- [Moment.js Documentation](https://momentjs.com/docs/) - מידע על ספריית Moment.js לניהול תאריכים.
- [Date-fns Documentation](https://date-fns.org/docs/Getting-Started) - מידע על ספריית date-fns לעבודה עם תאריכים.
