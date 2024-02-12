---
title:                "פרסום תאריך ממחרוזת"
aliases:
- he/javascript/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:15:07.328278-07:00
model:                 gpt-4-0125-preview
simple_title:         "פרסום תאריך ממחרוזת"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
פענוח תאריך ממחרוזת מאפשר למתכנתים להמיר ייצוגים טקסטואליים של תאריכים לאובייקטי `Date` ב-JavaScript, דבר המקל על מניפולציות, השוואות ופעולות פורמטינג של תאריכים. תהליך זה הוא חיוני לטיפול בקלט משתמש, עיבוד נתונים מבסיסי נתונים או עבודה עם ממשקי API שמתקשרים תאריכים בפורמטים של מחרוזות.

## איך לעשות:
JavaScript מציע באופן טבעי את המתודה `Date.parse()` ואת בנאי ה`Date` לפענוח מחרוזות תאריך. עם זאת, לשיטות אלו יש מגבלות וחוסר עקביות בין דפדפנים שונים, במיוחד עם פורמטים לא סטנדרטיים של תאריכים. כדי לטפל בבעיות אלו, ספריות צד שלישי כמו `Moment.js` ו-`date-fns` פופולריות בזכות העמידות והנוחות שלהן לשימוש.

### באמצעות JavaScript טבעי:
```javascript
const dateString = "2023-04-30T14:55:00";
const dateObj = new Date(dateString);

console.log(dateObj);  // פלט: Sun Apr 30 2023 14:55:00 GMT+0000 (זמן אוניברסלי מתואם)
```

### באמצעות Moment.js:
ראשית, התקינו את Moment.js באמצעות npm או כללו אותו בפרויקט שלכם. אז:
```javascript
const moment = require('moment');

const dateString = "2023-04-30T14:55:00";
const dateObj = moment(dateString);

console.log(dateObj.toString());  // פלט: Sun Apr 30 2023 14:55:00 GMT+0000
```

### באמצעות date-fns:
לאחר שתוסיפו את `date-fns` לפרויקט שלכם, פרשו מחרוזת תאריך כך:
```javascript
const { parseISO } = require('date-fns');

const dateString = "2023-04-30T14:55:00";
const dateObj = parseISO(dateString);

console.log(dateObj);  // פלט: 2023-04-30T14:55:00.000Z
```

ספריות כמו `Moment.js` ו-`date-fns` מספקות יכולות פרשנות רחבות יותר, כולל התמודדות עם מגוון פורמטים ולוקליזציות, מה שהופך אותן למועדפות עבור יישומים מורכבים.
