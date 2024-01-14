---
title:                "Javascript: מחיקת תווים התואמים דפוס"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## למה
מניעה נוספת לתמידת תווים המתאימים לתבנית יכולה להאיץ תהליך קוד ולמנוע באגים בקוד.

## איך לבצע
כאשר אתה רוצה למחוק תווים המתאימים לתבנית מזווית שונה מתבנית מסוימת, ניתן להשתמש בפונקציה "replace()" כדי להמיר את התווים הנדרשים למחרוזת ריקה. לדוגמה, אם אתה רוצה למחוק את כל האותיות הקטנות מתוך מחרוזת, ניתן להשתמש בתבנית "/[a-z]/g" כדי למצוא ולמחוק אותן. ניתן להשתמש גם בתבניתים אחרות, תלוי בצורך שלך.

```Javascript
let str = "Hello World!";
str = str.replace(/[a-z]/g, ''); // output: "H W!"
```

## מעמד עמוק
השתמשת בפקודת "replace()" על מנת למחוק תווים המתאימים לתבנית באופן מהיר ונוח. אבל חשוב לזכור שהפונקציה תחזיר מחרוזת חדשה ולא תשנה את המחרוזת המקורית שלך. כמו כן, ניתן להשתמש בתבניות המתאימות למיקומי התווים במחרוזת כגון "/\d/" כדי למצוא ולמחוק את כל המספרים.

## ראה גם
- [מידע נוסף על פקודת "replace()"](https://developer.mozilla.org/he/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [פונקציות מובנות נוספות ב-Javascript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp)