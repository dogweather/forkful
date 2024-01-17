---
title:                "חילוץ תת-מחרוזות"
html_title:           "TypeScript: חילוץ תת-מחרוזות"
simple_title:         "חילוץ תת-מחרוזות"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## מה ולמה?
הפרידה של תדרים (או substrings באנגלית) היא תהליך שבו מתבצעת הפרידה של מחרוזות לחלקים קטנים יותר בהתאם לתנאים מסוימים. תהליך זה נעשה על ידי מתכנתים כדי לעבד מחרוזות עם יותר יעילות וכדי להפוך את הקוד לקריא יותר.

## איך לעשות?
תחת תבניות ```TypeScript ... ``` ישנם כמה דוגמאות לשימוש בפונקציות הקשורות להפרידה של תדרים ולפלט הנתונים המתקבלים.

### דוגמאות בשפת TypeScript:
```
// פיצול מחרוזת לפי מספר שלם
const str: string = 'Hello World!';
const sub1 = str.substring(0, 5); // sub1 = 'Hello'

// פיצול מחרוזת לפי מספר תווים מהסוף
const sub2 = str.substring(-3); // sub2 = 'rld!'

// חיתוך מחרוזת לתוך מערך
const arr: string[] = str.split(' '); // arr = ['Hello', 'World!']

// איחוד מספר מחרוזות לתוך מחרוזת אחת
const newStr = ['My', 'name', 'is'].join(' '); // newStr = 'My name is'
```

### דוגמאות בשפת JavaScript:
```
// פיצול מחרוזת לפי אות בודדת
let str = 'Hello World!';
let sub1 = str.substr(2, 5); // sub1 = 'llo W'

// קפיצת התו הראשון במחרוזת
let sub2 = str.substr(-2); // sub2 = 'd!'

// חיתוך מחרוזת לתוך מערך
let arr = str.split(' '); // arr = ['Hello', 'World!']

// איחוד מחרוזת ומספר לתוך מחרוזת אחת
let newStr = 'I have ' + 5 + ' apples.'; // newStr = 'I have 5 apples.'
```

## חפירה עמוקה
התהליך של הפרידה של תדרים הוא חלק חשוב בעיבוד של מחרוזות וקיים כבר עוד מתחילת התכנות. בשפת תכנות "C" כבר מספר שנים, פונקציות כמו "substring" ו"substr" היו קיימות והשתמשו ביותר מתקנים כגון "char" ו-"char*" לעיבוד תדרים. בשנים האחרונות, עם התפתחות שפת JavaScript וכיוונה לשפת TypeScript מודרנית יותר, יצאו פונקציות כמו "substring" ו-"split" שניתנות לשימוש יותר נוח בעבודה עם מחרוזות. בנוסף, ישנם גם אלגוריתמים יעילים יותר כמו Regex (ביטויים רגולריים) שיכולים לעבוד עם מחרוזות כדי להפיק תוצאה דמויתה לשימוש בתהליך הפרידה של תדרים.

## ראו גם
למידע נוסף על פונקציות של תדרים בשפות תכנות נוספות:
- [פונקציות של תדרים בשפת C](https://www.tutorialspoint.com/c_standard_library/c_function_str.htm)
- [פונקציות של תדרים בשפת JavaScript](https://developer.mozilla.org/he/docs/Web/JavaScript/Reference/Global_Objects/String)