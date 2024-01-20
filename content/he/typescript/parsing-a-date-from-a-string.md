---
title:                "פענוח תאריך ממחרוזת"
html_title:           "Bash: פענוח תאריך ממחרוזת"
simple_title:         "פענוח תאריך ממחרוזת"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?

זיהוי תאריך ממחרוזת הוא התהליך של קריאה של תאריך כתמליל והמרתו לתאריך שיש לשפת התכנות להבין. תכנתים לעיתים קורות זקוקים לזיהוי תאריך ממחרוזת כאשר הם מתמודדים עם הכנסת כניסה מהמשתמש או מייבא מקורות נתונים שאינם מסודרים.

## איך:
TypeScript מספק את המחלקה `Date` לזיהוי תאריך ממחרוזת. להלן דוגמה איך להשתמש בה:

```TypeScript
let myDateString = "2020-02-29T12:34:56";
let myDate = new Date(myDateString);
console.log(myDate);
```

הזיהוי תהליך יתבצע אוטומטית.

## צלילה עמוקה:

בהקשר ההיסטורי, המתודולוגיה של זיהוי תאריך נמשכת כבר זמן רב. בחלק מהשפות, התהליך היה מאוד מסורבל. TypeScript, אף על פי שהוא HTML/JavaScript, מציע את הפלטפורמה הסטנדרטית של JavaScript והופך את התהליך לפשוט יותר.

תחליפים לשיטה הזו כוללים שימוש בחבילות צד שלישי כמו Moment.js אך הבעיה שלהם היא שהם מביאים עמם תלות.

כאשר TypeScript מנתח תאריך ממחרוזת, הוא משתמש בפונקציה העולמית של JavaScript `Date.parse()` כמנגנון הזיהוי. זה מאפשר לשפה לטפל במגוון פורמטים לגבי ההזנה.

## ראה גם:

JavaScript Date Reference - https://www.w3schools.com/js/js_date_methods.asp

Moment.js - https://momentjs.com/ 

TypeScript Date - https://www.typescriptlang.org/docs/handbook/utility-types.html#datetimelike

מדריך של TypeScript בנושא פעולות ומתודות תאריך.