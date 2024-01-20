---
title:                "המרת תאריך למחרוזת"
html_title:           "Bash: המרת תאריך למחרוזת"
simple_title:         "המרת תאריך למחרוזת"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
תהליך המרת תאריך למחרוזת מאפשר למתכנתים להציג את התאריך בפורמט מותאם אישית, מה שהופך אותו לנוח יותר לקריאה.בנוסף, מרת מחרוזת לתאריך משמשת לשמירה ושליחה נוחה של נתונים בין השרתים.

## איך לעשות:
קוד קטן שממחיש את התהליך:
```Javascript
let date = new Date();
let strDate = date.toString();
console.log(strDate); // Outputs: 'Wed Mar 17 2021 12:20:00 GMT-0700 (Pacific Daylight Time)'
```
כאן, אנחנו משתמשים במתודה `.toString()` של אובייקט Date כדי להמיר את התאריך למחרוזת.
## טבילה עמוקה
המרת תאריך למחרוזת היא טכניקה שהתפתחה בהיסטוריה המוקדמת של התכנות, כאשר היה צורך להעביר נתונים בין מחשבים בצורה יעילה. יתר על כך, ישנן אלטרנטיבות לפונקציה `toString()`, כמו `toISOString()`, הממירה את התאריך למחרוזת בפורמט ISO8601.

## ראה גם:
- [MDN Web Docs - Date.prototype.toString()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toString)
- [MDN Web Docs - Date.prototype.toISOString()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toISOString)
- [StackOverflow - Convert js Date time to MySQL datetime](https://stackoverflow.com/questions/5129624/convert-js-date-time-to-mysql-datetime)