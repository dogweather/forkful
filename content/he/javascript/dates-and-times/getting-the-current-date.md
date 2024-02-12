---
title:                "קבלת התאריך הנוכחי"
date:                  2024-02-03T19:10:28.383482-07:00
model:                 gpt-4-0125-preview
simple_title:         "קבלת התאריך הנוכחי"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
קבלת התאריך הנוכחי ב-JavaScript היא משימה בסיסית, הכוללת את אחזור ולעיתים גם שינוי של תאריך ושעה של היום. מתכנתים מבצעים זאת כדי להציג תאריכים באתרי אינטרנט, ביישומים, לעקוב אחרי אינטרקציות של משתמשים, או לטפל בנתונים התלויים בזמן.

## איך לעשות:
ב-JavaScript וניל, האובייקט `Date` משמש לעבוד עם תאריכים וזמנים. הנה כיצד ניתן לקבל את התאריך והשעה הנוכחיים:

```javascript
const currentDate = new Date();
console.log(currentDate); // דוגמא לפלט: Fri Apr 14 2023 12:34:56 GMT+0100 (British Summer Time)
```

להצגת התאריך בלבד בפורמט ידידותי יותר למשתמש, ניתן להשתמש במתודות כמו `toLocaleDateString()`:

```javascript
console.log(currentDate.toLocaleDateString()); // דוגמא לפלט: 14/4/2023
```

לשליטה רבה יותר על הפורמט, ספריות של צד שלישי כמו *Moment.js* או *date-fns* הן פופולריות מאוד, אך כדאי לדעת כי Moment.js כעת נחשבת לפרויקט מורשת שנמצא במצב תחזוקה.

באמצעות *Moment.js*:

```javascript
const moment = require('moment'); // בהנחה שמשתמשים ב-Node.js או באמצעות מאגד מודולים
const formattedDate = moment().format('YYYY-MM-DD');
console.log(formattedDate); // דוגמא לפלט: 2023-04-14
```

עם *date-fns*, המדגישה מודולריות שמאפשרת לכם לייבא רק את מה שאתם צריכים:

```javascript
const { format } = require('date-fns');
const formattedDate = format(new Date(), 'yyyy-MM-dd');
console.log(formattedDate); // דוגמא לפלט: 2023-04-14
```

כל שיטה מציעה רמות שונות של נוחות וגמישות לעבודה עם תאריכים ב-JavaScript, החל מהאובייקט `Date` המובנה ועד ליכולות עיצוב ושינוי מורכבות יותר הזמינות באמצעות ספריות.
