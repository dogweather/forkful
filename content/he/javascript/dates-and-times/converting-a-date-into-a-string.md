---
date: 2024-01-20 17:36:56.868302-07:00
description: "\u05D4\u05DE\u05E8\u05EA \u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05D1-JavaScript \u05DE\u05D0\u05E4\u05E9\u05E8\u05EA\
  \ \u05DC\u05D9\u05E6\u05D5\u05E8 \u05EA\u05E6\u05D5\u05D2\u05D4 \u05D0\u05D3\u05DD\
  -\u05E7\u05E8\u05D9\u05D0\u05D4 \u05E9\u05DC \u05DE\u05D9\u05D3\u05E2 \u05E2\u05DC\
  \ \u05EA\u05D0\u05E8\u05D9\u05DA \u05D5\u05E9\u05E2\u05D4. \u05EA\u05D5\u05DB\u05E0\
  \u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\
  \u05D4 \u05DB\u05D3\u05D9 \u05DC\u05D4\u05E6\u05D9\u05D2 \u05D0\u05D5 \u05DC\u05E9\
  \u05DE\u05D5\u05E8 \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD \u05D1\u05E4\u05D5\
  \u05E8\u05DE\u05D8\u05D9\u05DD \u05DE\u05E1\u05D5\u05D9\u05DE\u05D9\u05DD."
lastmod: '2024-03-13T22:44:39.994306-06:00'
model: gpt-4-1106-preview
summary: "\u05D4\u05DE\u05E8\u05EA \u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA \u05D1-JavaScript \u05DE\u05D0\u05E4\u05E9\u05E8\u05EA\
  \ \u05DC\u05D9\u05E6\u05D5\u05E8 \u05EA\u05E6\u05D5\u05D2\u05D4 \u05D0\u05D3\u05DD\
  -\u05E7\u05E8\u05D9\u05D0\u05D4 \u05E9\u05DC \u05DE\u05D9\u05D3\u05E2 \u05E2\u05DC\
  \ \u05EA\u05D0\u05E8\u05D9\u05DA \u05D5\u05E9\u05E2\u05D4."
title: "\u05D4\u05DE\u05E8\u05EA \u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA"
weight: 28
---

## מה ולמה?
המרת תאריך למחרוזת ב-JavaScript מאפשרת ליצור תצוגה אדם-קריאה של מידע על תאריך ושעה. תוכניתנים עושים את זה כדי להציג או לשמור תאריכים בפורמטים מסוימים.

## איך לעשות:
במהלך השימוש ב-JavaScript, ישנם מספר דרכים להמיר תאריך למחרוזת. הנה דוגמא לאיך אפשר לעשות את זה:

```javascript
let currentDate = new Date();

// המרה למחרוזת באמצעות toString()
let dateStr1 = currentDate.toString();
console.log(dateStr1); // Mon Mar 13 2023 17:45:00 GMT+0200 (Israel Standard Time)

// המרה לפורמט ISO עם toISOString()
let dateStr2 = currentDate.toISOString();
console.log(dateStr2); // 2023-03-13T15:45:00.000Z

// המרה למחרוזת מקומית עם toLocaleString()
let dateStr3 = currentDate.toLocaleString('he-IL');
console.log(dateStr3); // 13/03/2023, 17:45:00
```

## עיון מעמיק
ההיסטוריה של המרת תאריכים ב-JavaScript עדינה ומשתנה. מטודות מובנות כמו `toString()`, `toISOString()`, ו`toLocaleString()` מציעות אפשרויות מובנות להמרה. פורמטים אלה הם תוצר של התקדמות כללית בתחום האינטרנט והשפה.

למה שימוש ב-`toISOString()`? זה יוצר מחרוזת בפורמט ISO 8601, שהיא נורמה בינלאומית לייצוג תאריכים וזמנים.

`toLocaleString()` נותן לנו את הגמישות לציין פורמט מותאם לאזור גאוגרפי (locale) ספציפי. אז למשל, למשתמשים בישראל, הפורמט יהיה `dd/mm/yyyy, hh:mm:ss`.

חשוב לכיר גם ספריות חיצוניות, כמו Moment.js או date-fns, שמספקות עוד יותר גמישות בהמרה ובעיבוד תאריכים, אבל הן מוסיפות תלות בקוד שלך.

## ראה גם
- [MDN Date reference](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date) - מסמך מדריך של עצמי התאריך ב-JavaScript.
- [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601) - מידע על תקן ISO 8601 לייצוג תאריכים וזמנים.
- [Moment.js](https://momentjs.com/) - ספרייה לעיבוד תאריכים ב-JavaScript.
- [date-fns](https://date-fns.org/) - ספרייה מודרנית לעבודה עם תאריכים ב-JavaScript.
