---
title:                "מחיקת תווים התואמים לתבנית"
html_title:           "Elixir: מחיקת תווים התואמים לתבנית"
simple_title:         "מחיקת תווים התואמים לתבנית"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## מה ולמה?
מחיקת תווים שתואמים תבנית היא הפעלה שבה אנו מסירים תווים ממחרוזת שנבחרים לפי ביטוי רגולרי (או תבנית שאתה מגדיר). תכנתים עשויים לעשות כך לניקוי נתונים, למניעת שגיאות שימוש במידע, או להכנה לניתוח נתונים.

## איך לעשות:
אפשרות למימוש מחיקת תווים מתאימים עם Javascript היא באמצעות המתודה `replace()` של String object. כאן יש דוגמה:

```Javascript
let str = "Programming in Javascript is fun!";
let pattern = /fun/gi;
let newStr = str.replace(pattern, "");

console.log(newStr); 
// Output: "Programming in Javascript is !"
```

בדוגמה הזו, כל תרחיש של המילה "fun" מוחק מהמחרוזת, והמחרוזת החדשה הנוצרת מדפיסה לקונסול.

## צולילה עמוקה:
המתודה `replace()` מוכרת בעיקר מגרסה 1.2 של Javascript, כאשר התבנית יכולה להיות מחרוזת או ביטוי רגולרי. דרך אחרת למחוק תווים מתאימים היא להשתמש בלולאת for לעבור על כל תו של המחרוזת, אך זה יכול להיות יותר מסובך ופחות יעיל. המתודה `replace()` מספקת דרך מהירה וחסכונית למחיקת תווים מתאימים ממחרוזת.

## ראה גם:
עיין במקורות של מוזילה למידע מעמיק בנושא תבניות ביטויי רגולרים ומתודת replace():

1. [ביטויים רגולריים ב-JavaScript](https://developer.mozilla.org/he/docs/Web/JavaScript/Guide/Regular_Expressions)

2. [מתודת String.prototype.replace()](https://developer.mozilla.org/he/docs/Web/JavaScript/Reference/Global_Objects/String/replace)