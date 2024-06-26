---
date: 2024-01-20 17:33:29.855537-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1\u05E1\u05D9\
  \u05E1 \u05D4\u05D4\u05E9\u05D5\u05D5\u05D0\u05D4 \u05D1-JavaScript \u05D4\u05D5\
  \u05D0 \u05DC\u05D4\u05DE\u05D9\u05E8 \u05D0\u05EA \u05D4\u05EA\u05D0\u05E8\u05D9\
  \u05DB\u05D9\u05DD \u05DC\u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05E9\u05DE\u05D9\
  \u05D9\u05E6\u05D2\u05D9\u05DD \u05D0\u05EA \u05D4\u05DE\u05D9\u05DC\u05D9\u05E9\
  \u05E0\u05D9\u05D5\u05EA \u05DE\u05D0\u05D6 \u05D4-1 \u05D1\u05D9\u05E0\u05D5\u05D0\
  \u05E8 1970 (epoch time), \u05D5\u05D0\u05D6 \u05DC\u05D4\u05E9\u05D5\u05D5\u05EA\
  \ \u05D0\u05EA \u05D4\u05DE\u05E1\u05E4\u05E8\u05D9\u05DD. \u05D9\u05E9\u05E0\u05DF\
  \ \u05D2\u05DD\u2026"
lastmod: '2024-04-05T22:50:54.054579-06:00'
model: gpt-4-1106-preview
summary: "\u05D1\u05E1\u05D9\u05E1 \u05D4\u05D4\u05E9\u05D5\u05D5\u05D0\u05D4 \u05D1\
  -JavaScript \u05D4\u05D5\u05D0 \u05DC\u05D4\u05DE\u05D9\u05E8 \u05D0\u05EA \u05D4\
  \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD \u05DC\u05DE\u05E1\u05E4\u05E8\u05D9\u05DD\
  \ \u05E9\u05DE\u05D9\u05D9\u05E6\u05D2\u05D9\u05DD \u05D0\u05EA \u05D4\u05DE\u05D9\
  \u05DC\u05D9\u05E9\u05E0\u05D9\u05D5\u05EA \u05DE\u05D0\u05D6 \u05D4-1 \u05D1\u05D9\
  \u05E0\u05D5\u05D0\u05E8 1970 (epoch time), \u05D5\u05D0\u05D6 \u05DC\u05D4\u05E9\
  \u05D5\u05D5\u05EA \u05D0\u05EA \u05D4\u05DE\u05E1\u05E4\u05E8\u05D9\u05DD."
title: "\u05D4\u05E9\u05D5\u05D5\u05D0\u05EA \u05E9\u05EA\u05D9 \u05EA\u05D0\u05E8\
  \u05D9\u05DB\u05D9\u05DD"
weight: 27
---

## איך לעשות:
```javascript
// יצירת שני אובייקטים חדשים של תאריכים
let date1 = new Date(2023, 3, 24); // 24 אפריל 2023
let date2 = new Date(2023, 3, 25); // 25 אפריל 2023

// השוואה אם תאריך אחד קודם לאחר
if (date1 < date2) {
  console.log("date1 is before date2");
} else if (date1 > date2) {
  console.log("date1 is after date2");
} else {
  console.log("date1 is the same as date2");
}
```
פלט דוגמה:
```
date1 is before date2
```

## עיון מעמיק
בסיס ההשוואה ב-JavaScript הוא להמיר את התאריכים למספרים שמייצגים את המילישניות מאז ה-1 בינואר 1970 (epoch time), ואז להשוות את המספרים. ישנן גם ספריות חיצוניות, כמו Moment.js או Date-fns שמקלות על המשימה, אבל לעתים יספיקה הפונקציונליות הפשוטה של JS הרגיל לרוב הצורכים.

## ראה גם
- [MDN - Date - JavaScript | MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Date-fns - מודרני וקל מאוד לשימוש ספרייה לניהול תאריכים](https://date-fns.org/)
