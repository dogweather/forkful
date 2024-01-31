---
title:                "שימוש בביטויים רגולריים"
date:                  2024-01-19
html_title:           "Bash: שימוש בביטויים רגולריים"
simple_title:         "שימוש בביטויים רגולריים"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## מה ולמה?
רגולר אקספרשנז (ביטויים רגולריים) הן כלי חזקים לתיאור וחיפוש תבניות בטקסט. מתכנתים משתמשים בהם כדי לוודא קלט, לחפש ולהחליף מחרוזות, ולנתח נתונים ביעילות.

## איך לעשות:
```javascript
// חיפוש מחרוזת
let text = "יש כאן מקום לעוד מחרוזת";
let regex = /מחרוזת/;
console.log(regex.test(text)); // תוצאה: true

// חילוף מחרוזות
let newText = text.replace(/מחרוזת/, "מידע");
console.log(newText); // תוצאה: יש כאן מקום לעוד מידע

// בדיקת פורמט טלפון
let phonePattern = /^0\d([\d]{0,1})([-]{0,1})\d{7}$/;
let phone = "050-1234567";
console.log(phonePattern.test(phone)); // תוצאה: true
```

## עומק השיחה:
ביטויים רגולריים התפתחו בשנות ה-50 ומאז הם חלק אינטגרלי בתכנות. ישנן כמה שפות תכנות וכלים לניתוח טקסט אחרים, כמו XPath לעבודה עם XML. ב-JavaScript, יישום רגולר אקספרשנז מתבצע באמצעות המחלקה RegExp ומתודות כגון test ו-replace המוגדרות על מחרוזות.

## ראו גם:
- [MDN Web Docs - Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [RegExp Testing Tool](https://regexr.com/)
- [JavaScript RegExp Reference](https://www.w3schools.com/jsref/jsref_obj_regexp.asp)
