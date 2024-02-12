---
title:                "השוואת שתי תאריכים"
aliases:
- he/javascript/comparing-two-dates.md
date:                  2024-01-20T17:33:29.855537-07:00
model:                 gpt-4-1106-preview
simple_title:         "השוואת שתי תאריכים"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/comparing-two-dates.md"
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
