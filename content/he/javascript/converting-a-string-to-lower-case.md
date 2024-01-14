---
title:    "Javascript: המרת מחרוזת לאותיות קטנות"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## מדוע

המרת מחרוזת לאותיות קטנות היא תהליך חשוב בתכנות ב-Javascript. לעיתים, כאשר אנו מטפלים בטקסטים משתנים, נדרשת לנו תצוגה יחידה של הטקסט, והשתמשות באותיות קטנות מאפשרת לנו להגיע לתוצאה זו. כמו כן, בכמה מקרים, ממשקי משתמש ומערכות יכולים לטפל במחרוזות רק כאשר הן באותיות קטנות. מכאן נובעת חשיבות המרת מחרוזות לאותיות קטנות כפתרון מקובל ופשוט לבעיות של התמתנות עם טקסטים.

## איך לעשות זאת

הפעולה הבסיסית של המרת מחרוזת לאותיות קטנות ב-Javascript נעשית על ידי שימוש במתודה המובנית `toLowerCase()`. ניתן להשתמש במתודה זו כדי להמיר את המחרוזת לאותיות קטנות, ולשמור על המחרוזת המקורית כמה שנרצה. לדוגמה:

```Javascript
let text = "HELLO WORLD";
console.log(text.toLowerCase());
// Output: hello world
```

בנוסף, ניתן להשתמש במתודה זו כדי לקרוא להמיר את המחרוזת לאותיות קטנות בזמן ריצה של התוכנית. לדוגמה:

```Javascript
let text = prompt("Enter a sentence:");
console.log(text.toLowerCase());
// User enters: HELLO WORLD
// Output: hello world
```

## מעמיקים יותר

המתודה `toLowerCase()` מוגדרת על ידי התקן [ECMAScript](https://tc39.es/ecma262/), ששומר כחלק מתקני תוכנית מספר בתנאי בינלאומיים. המתודה מחזירה ערך חדש של המחרוזת עם כל האותיות באותיות קטנות. לעיתים, אם אין נתיב תקין לתמונה, המתודה עשויה להחזיר `NaN` כתוצאה, מה שמצפין "לא מספר".

כפי שמתכתב בהמעוף בתיקיה "[המרת מחרוזת לאותיות גדולות א