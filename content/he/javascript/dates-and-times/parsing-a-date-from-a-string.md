---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:07.328278-07:00
description: "\u05E4\u05E2\u05E0\u05D5\u05D7 \u05EA\u05D0\u05E8\u05D9\u05DA \u05DE\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DE\u05D0\u05E4\u05E9\u05E8 \u05DC\u05DE\
  \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DC\u05D4\u05DE\u05D9\u05E8 \u05D9\u05D9\
  \u05E6\u05D5\u05D2\u05D9\u05DD \u05D8\u05E7\u05E1\u05D8\u05D5\u05D0\u05DC\u05D9\u05D9\
  \u05DD \u05E9\u05DC \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD \u05DC\u05D0\u05D5\
  \u05D1\u05D9\u05D9\u05E7\u05D8\u05D9 `Date` \u05D1-JavaScript, \u05D3\u05D1\u05E8\
  \ \u05D4\u05DE\u05E7\u05DC \u05E2\u05DC \u05DE\u05E0\u05D9\u05E4\u05D5\u05DC\u05E6\
  \u05D9\u05D5\u05EA, \u05D4\u05E9\u05D5\u05D5\u05D0\u05D5\u05EA \u05D5\u05E4\u05E2\
  \u05D5\u05DC\u05D5\u05EA \u05E4\u05D5\u05E8\u05DE\u05D8\u05D9\u05E0\u05D2 \u05E9\
  \u05DC\u2026"
lastmod: 2024-02-19 22:04:59.263334
model: gpt-4-0125-preview
summary: "\u05E4\u05E2\u05E0\u05D5\u05D7 \u05EA\u05D0\u05E8\u05D9\u05DA \u05DE\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05DE\u05D0\u05E4\u05E9\u05E8 \u05DC\u05DE\u05EA\
  \u05DB\u05E0\u05EA\u05D9\u05DD \u05DC\u05D4\u05DE\u05D9\u05E8 \u05D9\u05D9\u05E6\
  \u05D5\u05D2\u05D9\u05DD \u05D8\u05E7\u05E1\u05D8\u05D5\u05D0\u05DC\u05D9\u05D9\u05DD\
  \ \u05E9\u05DC \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD \u05DC\u05D0\u05D5\u05D1\
  \u05D9\u05D9\u05E7\u05D8\u05D9 `Date` \u05D1-JavaScript, \u05D3\u05D1\u05E8 \u05D4\
  \u05DE\u05E7\u05DC \u05E2\u05DC \u05DE\u05E0\u05D9\u05E4\u05D5\u05DC\u05E6\u05D9\
  \u05D5\u05EA, \u05D4\u05E9\u05D5\u05D5\u05D0\u05D5\u05EA \u05D5\u05E4\u05E2\u05D5\
  \u05DC\u05D5\u05EA \u05E4\u05D5\u05E8\u05DE\u05D8\u05D9\u05E0\u05D2 \u05E9\u05DC\
  \u2026"
title: "\u05E4\u05E8\u05E1\u05D5\u05DD \u05EA\u05D0\u05E8\u05D9\u05DA \u05DE\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA"
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
