---
title:                "הדפסת פלט ניפוי שגיאות"
html_title:           "Arduino: הדפסת פלט ניפוי שגיאות"
simple_title:         "הדפסת פלט ניפוי שגיאות"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

## מה ולמה?

- הדפסת פלט לניפוי שגיאות היא כלי שהמתכנת משתמש בו כדי לבחון את הקוד שלו למציאת שגיאות. זה מסייע לנו להבין את התהליך לפיו מתבצע קוד מסוים תוך כדי שהתוכנית מריצה אותו, ולאתר בדיוק במקום שבו מתרחש הבעיה.

## איך ל:

הדוגמה הנפוצה ביותר של הדפסת שגיאות ב-JavaScript היא על ידי שימוש בפונקציה console.log():

```Javascript
var a = 5;
var b = 6;
console.log(a+b); // הדפסת 11
```
כאן, אתה מדפיס את תוצאת הצירוף של a ו-b לקונסולה.

## צלילה עמוקה:

אמנם console.log() היא הרגילה ביותר והנפוצה בשימוש אבל ישנם פריסות מתקדמות יותר של זרימת פלט לצורך ניפוי. כמו console.debug(), console.error() ו-console.warn() שאיך לכל אחת מהן מאפיינים שונים. 

היסטורית, גם טכניקת ה-commenting הייתה שיטה בה נעשה שימוש כדי לנפות שגיאות. אבל זה יכול להוסיף מנגנונים לא מסודרים בקוד שלך.

## ראה גם:

 - [מדריכים לניפוי ב-MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide)
 - [מסמך ניפוי JavaScript בקונסולת Chrome Dev](https://developer.chrome.com/docs/devtools/javascript/)