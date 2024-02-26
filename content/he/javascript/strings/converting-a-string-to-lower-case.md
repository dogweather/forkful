---
date: 2024-01-20 17:39:07.631931-07:00
description: "\u05D4\u05DE\u05E8\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\
  \u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA \u05D4\u05D9\
  \u05D0 \u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05D1\u05D5 \u05D0\u05E0\u05D5 \u05DE\
  \u05E9\u05E0\u05D9\u05DD \u05D0\u05EA \u05DB\u05DC \u05D4\u05EA\u05D5\u05D5\u05D9\
  \u05DD \u05D1\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\u05D0\u05D5\u05EA\u05D9\
  \u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA. \u05E4\u05E8\u05D5\u05D2\u05E8\u05DE\
  \u05E8\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\
  \u05D9 \u05DC\u05D0\u05D7\u05D3 \u05E4\u05D5\u05E8\u05DE\u05D8\u05D9\u05DD, \u05DC\
  \u05D4\u05E9\u05D5\u05D5\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05D1\
  \u05DC\u05D9 \u05DC\u05D4\u05EA\u05D7\u05E9\u05D1\u2026"
lastmod: '2024-02-25T18:49:38.195232-07:00'
model: gpt-4-1106-preview
summary: "\u05D4\u05DE\u05E8\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\u05D0\
  \u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA \u05D4\u05D9\u05D0\
  \ \u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05D1\u05D5 \u05D0\u05E0\u05D5 \u05DE\u05E9\
  \u05E0\u05D9\u05DD \u05D0\u05EA \u05DB\u05DC \u05D4\u05EA\u05D5\u05D5\u05D9\u05DD\
  \ \u05D1\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\u05D0\u05D5\u05EA\u05D9\u05D5\
  \u05EA \u05E7\u05D8\u05E0\u05D5\u05EA. \u05E4\u05E8\u05D5\u05D2\u05E8\u05DE\u05E8\
  \u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9\
  \ \u05DC\u05D0\u05D7\u05D3 \u05E4\u05D5\u05E8\u05DE\u05D8\u05D9\u05DD, \u05DC\u05D4\
  \u05E9\u05D5\u05D5\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05D1\u05DC\
  \u05D9 \u05DC\u05D4\u05EA\u05D7\u05E9\u05D1\u2026"
title: "\u05D4\u05DE\u05E8\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\u05D0\
  \u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
המרת מחרוזת לאותיות קטנות היא תהליך שבו אנו משנים את כל התווים במחרוזת לאותיות קטנות. פרוגרמרים עושים זאת כדי לאחד פורמטים, להשוות מחרוזות בלי להתחשב ברישיות, או לפשט את עיבוד הטקסט.

## איך לעשות:
קטע קוד פשוט ב-JavaScript:

```javascript
let greeting = "שלום עולם!";
let lowerCaseGreeting = greeting.toLowerCase();
console.log(lowerCaseGreeting); // פלט: "שלום עולם!"
```

## צלילה לעומק:
בימים הראשונים של התכנות, אנשים לא תמיד קיוו לאחידות ברישיות האותיות. בעולם המודרני, עבור השוואות טקסט, כמו בסיסי נתונים או מנועי חיפוש, המשמעות של אחידות היא עצומה.

אלטרנטיבה אחת:
```javascript
let message = "היי מה קורה?";
let lowerCaseMessage = message.replace(/[A-Z]/g, char => char.toLowerCase());
console.log(lowerCaseMessage); // פלט: "היי מה קורה?"
```
אבל `toLowerCase()` זו הדרך הקלה והיותר יעילה.

ברמת המימוש:
הפונקציה `toLowerCase()` ב-JavaScript משתמשת בטבלאות ייחוס מובנות כדי למצוא את המקבילה הקטנה של כל אות גדולה. עבור שפות עם סטים גדולים של תווים, כמו העברית, המרה זו יכולה להיות יותר מורכבת, אבל המנועים המודרניים מכסים זאת היטב.

## ראה גם:
- מסמך MDN על `toLowerCase()`: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase
- עבודה עם רגולר אקספרשנס (Regular Expressions) ב-JavaScript: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions
- מדריך לאחידות קוד ב-JavaScript (Code Consistency): https://google.github.io/styleguide/jsguide.html
