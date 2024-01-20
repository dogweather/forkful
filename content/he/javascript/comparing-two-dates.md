---
title:                "השוואה בין שני תאריכים"
html_title:           "Arduino: השוואה בין שני תאריכים"
simple_title:         "השוואה בין שני תאריכים"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## מה ולמה?
השוואה של שני תאריכים היא תהליך שבו משווים בין שני תאריכים כדי לראות איזה מהם קורה קודם. מתכנתים עשויים לעשות זאת כדי לבצע תהליכים מסוימים בהתאם לרצף החלופי של אירועים שקרו בתאריכים שונים.

## כיצד לבצע:
הנה דרך פשוטה להשוות בין שני תאריכים ב-Javascript:
```Javascript
//הגדרת שני תאריכים
let date1 = new Date('2021-05-15');
let date2 = new Date('2022-01-20');

//השוואת התאריכים
if(date1 > date2) {
    console.log("date1 is later than date2");
} else if(date2 > date1) {
    console.log("date2 is later than date1");
} else {
    console.log("Both dates are equal");
}
```
הפלט מתוך זה יהיה: `"date2 is later than date1"`

## צלילה מעמיקה:
JavaScript אינו מכיל מתודה מובנית להשוואת תאריכים, אך התאריכים יכולים להיהפך למספרים עשרוניים שמייצגים את המיליסקנודות שחלפו מ-1 בינואר 1970. הודות לכך, ובאופן נפשר, ניתן להשתמש באופראטורים מתמטיים פשוטים מאוד להשוואת בין התאריכים.
ישנן חלופות אחרות, כמו להשתמש בספריות חיצוניות כמו moment.js, עם הן כמעט תמיד נותנות למשתמש את נוחות השימוש של מתודות מובנות להשוואת תאריכים.

## ראו גם:
- Moment.js: <a href="https://momentjs.com/" target="_blank">מסמך התיעוד</a>
- MDN Web Docs: <a href="https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date" target="_blank">חלק התאריכים</a>
- Javascript.info: <a href="https://javascript.info/date" target="_blank">שיעורים של JavaScript על תאריכים ושעות</a>