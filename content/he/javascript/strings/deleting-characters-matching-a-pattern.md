---
title:                "מחיקת תווים התואמים לתבנית"
aliases:
- /he/javascript/deleting-characters-matching-a-pattern.md
date:                  2024-01-20T17:42:49.438359-07:00
model:                 gpt-4-1106-preview
simple_title:         "מחיקת תווים התואמים לתבנית"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## מה ולמה?
מחיקת תווים שתואמים לתבנית היא פעולה שבה אנו מסירים חלקים ממחרוזת שעומדת בקריטריונים מסוימים. תכנתנים עושים זאת לטהר קלט, לאמת נתונים או לעיבוד מלל לפני שמישהו ישתמש בו.

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
