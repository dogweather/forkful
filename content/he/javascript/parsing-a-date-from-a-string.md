---
title:                "פענוח תאריך ממחרוזת"
html_title:           "Bash: פענוח תאריך ממחרוזת"
simple_title:         "פענוח תאריך ממחרוזת"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
חילוץ תאריך ממחרוזת הוא פעולה שבה מנתחים תאריך מתוך מחרוזת טקסט, ממירים אותו לפורמט התאריך המקובל. תוכניתים מבצעים את הפעולה הזו כדי להפוך מידע מקלטת טקסט מרובה סוגים לאובייקט תאריך שנשלוט בו בקלות יותר.

## איך לעשות:
```Javascript
let dateString = "2022-05-04";
let myDate = new Date(dateString);
console.log(myDate);
```
הפלט של הקוד הזה יהיה התאריך 'Wed May 04 2022 00:00:00 GMT+0000 (Coordinated Universal Time)'.

אפשר לנתח את הזמן של היום יחד עם התאריך:
```Javascript
let dateString = "2022-05-04T14:30:00.000Z";
let myDate = new Date(dateString);
console.log(myDate);
```
הפלט של הקוד הזה יהיה התאריך והשעה 'Wed May 04 2022 14:30:00 GMT+0000 (Coordinated Universal Time)'.

## צלילה עמוקה
ג'אווה סקריפט מאפשרת פעולה יחסית קלה של המרה של מחרוזות לתאריכים, למרות האתגרים שעלולים להינתן כתוצאה משפות, אזורים זמן ופורמטים שונים.
- **ההיסטוריה**: בהתחלה, המרת מחרוזות לתאריכים לא הייתה מרובה שימוש כפי שהיא היום. עם התפתחות הטכנולוגיה והתרבות הדיגיטלית, המרת מחרוזות לתאריכים הפכה לבסיס לעיבוד נתונים.
- **חלופות**: קיימות ספריות חיצוניות כמו moment.js או date-fns שמציעות יכולות חילוץ ותחשיב עם תאריכים מרובות.
- **פרטי ביצוע**: JavaScript משתמשת באובייקט Date הקיים אך מנתחת את המחרוזות באופן אוטומטי כאשר ישנן להן פורמט ISO 8601. 

## ראה גם
- [מסמכים על תאריכים מפרויקט MDN](https://developer.mozilla.org/he/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [ספריית Moment.js](https://momentjs.com/)
- [ספריית date-fns](https://date-fns.org/)