---
title:                "קבלת התאריך הנוכחי"
html_title:           "C#: קבלת התאריך הנוכחי"
simple_title:         "קבלת התאריך הנוכחי"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## מה ולמה?

מדובר בקבלת התאריך הנוכחי - כלומר, מידע על היום, החודש והשנה בזמן שבו מבצעים את הקוד. זה מועיל מכיוון שמאפשר לנו ליצור ציוצים ממוחשבים או טיפול באירועים הקרובים ביישומים מבוססי זמן.

## איך?

```TypeScript
let currentDate = new Date();
console.log(`התאריך הנוכחי הוא: ${currentDate}`);
```

ריצת הקוד הזה תחזיר לך את התאריך והשעה הנוכחיים.

## בתוך המקוש:

בעוד שקטע הקוד למעלה משתמש באביקט Date שמורת לכם (מה שמספיק לרוב התרחישים), שימו לב שהאובייקטים של JavaScript, בשום מקרה, אינם מתמודדום בצורה שלמה עם בעיות הזמן. נקודה נוספת היא שישנם כלים אחרים כמו Day.js ו moment.js שמספקים פעולות נוספות לניהול תאריכים.

אבל אם יש בהכרח צורך להתמודד עם איזורי זמן, תקציבים, קפיצה שעונית, אתה יכול לשקול להשתמש בספריות כמו luxon או date-fns.

## ראו גם:

- תיעוד JavaScript מה MDN: <https://developer.mozilla.org/he/docs/Web/JavaScript/Reference/Global_Objects/Date>

- מדריך שלמה ל- JavaScript Date: <https://flaviocopes.com/javascript-dates/>

- Moment.js: <https://momentjs.com/>

- Day.js: <https://day.js.org/>

- Luxon: <https://moment.github.io/luxon/>

- Date-fns: <https://date-fns.org/>.