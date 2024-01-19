---
title:                "השוואה בין שני תאריכים"
html_title:           "Arduino: השוואה בין שני תאריכים"
simple_title:         "השוואה בין שני תאריכים"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

# דייטים בטייפסקריפט: כיצד להשוות שני תאריכים

## מה ולמה?
השוואת תאריכים היא פעולה שמאפשרת לנו לראות איזה משני תאריכים הוא המוקדם יותר או המאוחר יותר. זה תוך שמירה על מעין "קרונולוגיה" של האירועים.

## כיצד:
הנה דרך פשוטה להשוות שני תאריכים בטייפסקריפט.
```TypeScript
let date1 = new Date(2021, 11, 24);
let date2 = new Date(2022, 11, 24);

if(date1 > date2) {
    console.log("date1 is later than date2");
} else if(date1 < date2) {
    console.log("date1 is earlier than date2");
} else {
    console.log("Both dates are same");
}
```
<div dir="rtl">
במידה ו-date1 מאוחר יותר מdate2 הפלט יהיה: "date1 is later than date2". אם date1 מוקדם יותר מdate2 ,הפלט יהיה: "date1 is earlier than date2". כאשר שני התאריכים שווים הפלט יהיה: "Both dates are same".

</div>

## השקיעה מעמיקה:
<div dir="rtl">
השוואת תאריכים היא מרכיב חיוני בתכנות המאפשרת לנו לבצע מגוון פעולות, כמו למשל לסדר אירועים לפי התרחשותם או לחשב מרווחי זמן. למעשה, מאז ימי פורטרן (שם של שפת תכנות עתיקה) אנשים השווו תאריכים. 
HTML5 ספק דרך חדשה וחכמה יותר לעבוד עם תאריכים דרך סוג ה-input שלו, נותן לנו את הכוח לעבוד עם שני טיפים של תאריכים, date ו-time, המאפשרת לנו להשוות תאריכים ולהדפיס אותם בפורמטים שונים ולא רק כ-strings.

</div>
## ראו גם:
- [נושא תאריכים בטייפסקריפט](https://developer.mozilla.org/he/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [תאריכים וזמנים ב JavaScript](https://learn.javascript.info/date)
- [הדרך הנכונה להשוות שניים תאריכים ב JavaScript](https://stackoverflow.com/questions/492994/compare-two-dates-with-javascript)