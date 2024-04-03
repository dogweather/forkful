---
date: 2024-01-20 17:42:49.438359-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: ."
lastmod: '2024-03-13T22:44:39.949077-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u05DE\u05D7\u05D9\u05E7\u05EA \u05EA\u05D5\u05D5\u05D9\u05DD \u05D4\u05EA\
  \u05D5\u05D0\u05DE\u05D9\u05DD \u05DC\u05EA\u05D1\u05E0\u05D9\u05EA"
weight: 5
---

## איך לעשות:
```javascript
// דוגמא למחיקת כל הספרות ממחרוזת
let string = 'Hello123 World456!';
let cleanedString = string.replace(/\d+/g, '');
console.log(cleanedString); // 'Hello World!'

// דוגמא למחיקת תווי פיסוק
let stringWithPunctuation = 'Hello, World!';
let noPunctuation = stringWithPunctuation.replace(/[.,\/#!$%\^&\*;:{}=\-_`~()]/g,"");
console.log(noPunctuation); // 'Hello World'
```

## עיון מעמיק:
מחיקת תווים לפי תבנית היא חלק מהיסודות של עבודה עם מחרוזות, וזה מתבצע בעזרת ביטויים רגולריים (Regular Expressions). הרעיון קיים משנות ה-50 והוא התפתח עם השנים. קיימות אלטרנטיבות למחיקת תווים ללא שימוש בביטויים רגולריים, כמו `slice`, `substring`, או `split` ו`join`, אבל אלה פחות גמישות. ביטויים רגולריים מאפשרים לך להגדיר תבנית מורכבת לחיפוש ולהחלפה, והם מהווים כלי עוצמתי ביותר לעיבוד טקסט.

## ראו גם:
- [MDN - Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [RegExp - JavaScript | MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp)
- [String.prototype.replace() - JavaScript | MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
