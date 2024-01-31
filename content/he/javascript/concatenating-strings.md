---
title:                "שרשור מחרוזות"
date:                  2024-01-20T17:35:06.361611-07:00
model:                 gpt-4-1106-preview
simple_title:         "שרשור מחרוזות"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## מה ולמה?
הדבקת מחרוזות היא פשוט לקחת שתי מחרוזות או יותר ולחבר אותן ביחד לאחת. תוכניתנים עושים את זה כדי לאחד טקסט, ליצור משפטים משתנים, או להוסיף דינמיות לתוכנית.

## איך לעשות:
```Javascript
// שיטה קלאסית ב-JavaScript
let greeting = "שלום";
let name = "עולם";
let message = greeting + " " + name + "!";
console.log(message); // יודפס: שלום עולם!

// שימוש ב-Template literals להדבקה נוחה יותר
let message2 = `${greeting} ${name}!`;
console.log(message2); // יודפס: שלום עולם!
```

## צלילה לעומק
בעבר, תוכניתנים השתמשו בסימן הפלוס (+) להדבקת מחרוזות. זה עבד, אבל לפעמים היה קשה לנהל את כל הרווחים והשורות החדשות. עם הגעת ES6 ב-2015, נוספו template literals (מחרוזות תבנית) שמאפשרים הכנסת משתנים וביטויים ישירות בתוך המחרוזת עצמה, דבר שמקל מאוד על התהליך. הדבקת מחרוזות עדיין עושה שימוש רב בזיהוי רכיבי UI, יצירת קובצי HTML דינמיים, ובניית שאילתות למסדי נתונים.

## ראו גם
- [MDN Web Docs: Template literals](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)
- [MDN Web Docs: String.prototype.concat()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
- [MDN Web Docs: Expressions and operators](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Expressions_and_Operators#String_operators)
