---
title:                "חיפוש והחלפת טקסט"
html_title:           "Elm: חיפוש והחלפת טקסט"
simple_title:         "חיפוש והחלפת טקסט"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## מה ולמה?

חיפוש והחלפת טקסט הם פעולות שמאפשרות לנו למצוא כל מילה או פרגמנט ולהחליף אותו בערך אחר. מתכנתים משתמשים בכך למשל בשדרוג codebase או בהתאמות של השפה.

## איך לממש?

```TypeScript
let str = "היי, איך אני משנה מחרוזת?";

// חיפוש והחלפה ישירה
let newStr = str.replace("מחרוזת", "string");
console.log(newStr);  
// Output: "היי, איך אני משנה string?"

// חיפוש והחלפה של כמה מופעים
let regex = /מחרוזת/g;
newStr = str.replace(regex, "string");

console.log(newStr); 
// Output: "היי, איך אני משנה string?"
```

## צלילה עמוקה

1. בהקשר היסטורי: בשנת 1995 נוסף הmethod `replace()` ל- JavaScript, כחלק מהstandard  ECMA-262.
2. אלטרנטיבות: ניתן להשתמש גם בmethods `split()` ו `join()` כדי לבצע משימות דומות. רוב המתכנתים מעדיפים `replace()`.
3. פרטי הרצה: `replace()` בTypeScript (כמו בJavaScript) מחזיר מחרוזת חדשה ולא משנה את המחרוזת המקורית.

## ראו גם:

1. [מסמך האפיון של ECMA-262](https://www.ecma-international.org/ecma-262/10.0/index.html#sec-string.prototype.replace)
2. [MDN Web Docs - String.prototype.replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)