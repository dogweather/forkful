---
title:                "הגדלת אות הראשונה במחרוזת"
html_title:           "Javascript: הגדלת אות הראשונה במחרוזת"
simple_title:         "הגדלת אות הראשונה במחרוזת"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
הפיכת מחרוזת לאותיות גדולות היא תהליך שבו משנים את גודל האותיות של מחרוזת מאותיות קטנות לאותיות גדולות. מתכנתים משתמשים בזה למעשה ומשנים את הגודל כדי להקל על הקריאה וליצור התאמה לדרישות עיצובית באפליקציה.

## איך לעשות
בהנחה שיש לך מחרוזת שרוצה להפוך לאותיות גדולות, תוכל לבצע את הפעולה הבאה:

```Javascript
let str = 'hello world';
str = str.toUpperCase();
```

כאן התוצאות:

```Javascript
console.log(str); // 'HELLO WORLD'
```

## התעמקות
מאז הוויזרד של IDG, הפך המרת מחרוזות לאותיות גדולות לשימוש מוכר ונפוץ. נוכחות זו נמשכה עד היום, והיא מאפשרת קריאה נוחה ואינטואיטיבית. קיימים גם דרכים חלופיות להפיכת מחרוזת לאותיות גדולות, כולל שימוש ב-builtin או שימוש ב-loop:

```Javascript
function toUpperCase(str) {
    let result = '';
    for (let i = 0; i < str.length; i++) {
        result += str[i].toUpperCase();
    }
    return result;
}
console.log(toUpperCase('hello world')); // 'HELLO WORLD'
```

## ראה גם
להלן קישורים לאתרים שתוכל למצוא פרטים נוספים על המרת מחרוזת לאותיות גדולות ב-JavaScript:

1. [MDN Web Docs: String.prototype.toUpperCase()](https://developer.mozilla.org/he/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
2. [JavaScript String toUpperCase() Method - W3Schools](https://www.w3schools.com/jsref/jsref_touppercase.asp)