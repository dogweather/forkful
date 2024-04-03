---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:28.383482-07:00
description: "\u05E7\u05D1\u05DC\u05EA \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05D4\
  \u05E0\u05D5\u05DB\u05D7\u05D9 \u05D1-JavaScript \u05D4\u05D9\u05D0 \u05DE\u05E9\
  \u05D9\u05DE\u05D4 \u05D1\u05E1\u05D9\u05E1\u05D9\u05EA, \u05D4\u05DB\u05D5\u05DC\
  \u05DC\u05EA \u05D0\u05EA \u05D0\u05D7\u05D6\u05D5\u05E8 \u05D5\u05DC\u05E2\u05D9\
  \u05EA\u05D9\u05DD \u05D2\u05DD \u05E9\u05D9\u05E0\u05D5\u05D9 \u05E9\u05DC \u05EA\
  \u05D0\u05E8\u05D9\u05DA \u05D5\u05E9\u05E2\u05D4 \u05E9\u05DC \u05D4\u05D9\u05D5\
  \u05DD. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05D1\u05E6\u05E2\u05D9\
  \u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D4\u05E6\u05D9\u05D2 \u05EA\
  \u05D0\u05E8\u05D9\u05DB\u05D9\u05DD \u05D1\u05D0\u05EA\u05E8\u05D9\u2026"
lastmod: '2024-03-13T22:44:39.992727-06:00'
model: gpt-4-0125-preview
summary: "\u05E7\u05D1\u05DC\u05EA \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05D4\u05E0\
  \u05D5\u05DB\u05D7\u05D9 \u05D1-JavaScript \u05D4\u05D9\u05D0 \u05DE\u05E9\u05D9\
  \u05DE\u05D4 \u05D1\u05E1\u05D9\u05E1\u05D9\u05EA, \u05D4\u05DB\u05D5\u05DC\u05DC\
  \u05EA \u05D0\u05EA \u05D0\u05D7\u05D6\u05D5\u05E8 \u05D5\u05DC\u05E2\u05D9\u05EA\
  \u05D9\u05DD \u05D2\u05DD \u05E9\u05D9\u05E0\u05D5\u05D9 \u05E9\u05DC \u05EA\u05D0\
  \u05E8\u05D9\u05DA \u05D5\u05E9\u05E2\u05D4 \u05E9\u05DC \u05D4\u05D9\u05D5\u05DD\
  ."
title: "\u05E7\u05D1\u05DC\u05EA \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05D4\u05E0\
  \u05D5\u05DB\u05D7\u05D9"
weight: 29
---

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
