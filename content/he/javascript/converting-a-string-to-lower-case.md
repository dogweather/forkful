---
title:                "המרת מחרוזת לאותיות קטנות"
date:                  2024-01-20T17:39:07.631931-07:00
model:                 gpt-4-1106-preview
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/converting-a-string-to-lower-case.md"
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
