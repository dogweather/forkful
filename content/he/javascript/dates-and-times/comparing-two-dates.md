---
date: 2024-01-20 17:33:29.855537-07:00
description: "\u05D4\u05E9\u05D5\u05D5\u05D0\u05EA \u05E9\u05EA\u05D9 \u05EA\u05D0\
  \u05E8\u05D9\u05DB\u05D9\u05DD \u05D1-JavaScript \u05D6\u05D4 \u05E4\u05E9\u05D5\
  \u05D8 \u05DC\u05D1\u05D3\u05D5\u05E7 \u05D0\u05DD \u05EA\u05D0\u05E8\u05D9\u05DA\
  \ \u05D0\u05D7\u05D3 \u05E7\u05D5\u05D3\u05DD, \u05D0\u05D7\u05E8\u05D9 \u05D0\u05D5\
  \ \u05E9\u05D5\u05D5\u05D4 \u05DC\u05EA\u05D0\u05E8\u05D9\u05DA \u05D0\u05D7\u05E8\
  . \u05EA\u05DB\u05E0\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\
  \u05D0\u05EA \u05DC\u05E1\u05D3\u05E8 \u05D0\u05D9\u05E8\u05D5\u05E2\u05D9\u05DD\
  , \u05D5\u05DC\u05D9\u05D3\u05E6\u05D9\u05D4 \u05E9\u05DC \u05D8\u05D5\u05D5\u05D7\
  \u05D9 \u05D6\u05DE\u05DF \u05D5\u05E2\u05D5\u05D3."
lastmod: '2024-03-13T22:44:39.995820-06:00'
model: gpt-4-1106-preview
summary: "\u05D4\u05E9\u05D5\u05D5\u05D0\u05EA \u05E9\u05EA\u05D9 \u05EA\u05D0\u05E8\
  \u05D9\u05DB\u05D9\u05DD \u05D1-JavaScript \u05D6\u05D4 \u05E4\u05E9\u05D5\u05D8\
  \ \u05DC\u05D1\u05D3\u05D5\u05E7 \u05D0\u05DD \u05EA\u05D0\u05E8\u05D9\u05DA \u05D0\
  \u05D7\u05D3 \u05E7\u05D5\u05D3\u05DD, \u05D0\u05D7\u05E8\u05D9 \u05D0\u05D5 \u05E9\
  \u05D5\u05D5\u05D4 \u05DC\u05EA\u05D0\u05E8\u05D9\u05DA \u05D0\u05D7\u05E8. \u05EA\
  \u05DB\u05E0\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\
  \u05EA \u05DC\u05E1\u05D3\u05E8 \u05D0\u05D9\u05E8\u05D5\u05E2\u05D9\u05DD, \u05D5\
  \u05DC\u05D9\u05D3\u05E6\u05D9\u05D4 \u05E9\u05DC \u05D8\u05D5\u05D5\u05D7\u05D9\
  \ \u05D6\u05DE\u05DF \u05D5\u05E2\u05D5\u05D3."
title: "\u05D4\u05E9\u05D5\u05D5\u05D0\u05EA \u05E9\u05EA\u05D9 \u05EA\u05D0\u05E8\
  \u05D9\u05DB\u05D9\u05DD"
---

{{< edit_this_page >}}

## מה ולמה?
השוואת שתי תאריכים ב-JavaScript זה פשוט לבדוק אם תאריך אחד קודם, אחרי או שווה לתאריך אחר. תכנתנים עושים זאת לסדר אירועים, ולידציה של טווחי זמן ועוד.

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
