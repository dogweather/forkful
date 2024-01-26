---
title:                "הפיכת מחרוזת לאותיות רישיות"
html_title:           "Bash: הפיכת מחרוזת לאותיות רישיות"
simple_title:         "הפיכת מחרוזת לאותיות רישיות"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
המרת מחרוזת לאותיות גדולות היא פעולה שמשנה כל אות במחרוזת לאות רישית (גדולה). פרוגרמרים עושים את זה לשם עקביות בממשק משתמש, הדפסת כותרות, או נירמול קלטים לבדיקות או לשמור נתונים.

## איך לעשות:
כדי להמיר מחרוזת לאותיות גדולות בJavaScript, קל להשתמש במתודה `.toUpperCase()`:

```Javascript
let myString = "שלום עולם!";
let capitalizedString = myString.toUpperCase();
console.log(capitalizedString); // "שלום עולם!"
```

פלט לדוגמא:
```
"שלום עולם!"
```

שימו לב: בעברית אין הבחנה בין אותיות גדולות לקטנות, אבל המתודה יעילה במיוחד עבור טקסט באנגלית.

## צלילה עמוקה
המרת טקסט לגרסת האותיות הגדולות שלו היא פונקציונליות פשוטה, אך חשובה, שנמצאת ברוב שפות התכנות. היא מאוד שימושית בעיבוד טקסט ובמערכות שבהן חשובה הקפדה על רישום עקבי של נתונים. לוקחות בחשבון רק א-ז ואינן בודקת שפות נוספות. אם עובדים עם שפות נוספות שבהן יש הבחנה בין אותיות קטנות וגדולות, יש להתאים את הקוד או למצוא פתרון מתאים נוסף. בעיקרון, כאשר עושים שימוש ב`.toUpperCase()`, חשוב לבדוק תמיד שהטקסט אכן תואם לתרבות ואופי השפה הרלוונטית.

## ראה גם
- [MDN Web Docs - String.prototype.toUpperCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [JavaScript.info - Strings](https://javascript.info/string)
- [W3Schools - JavaScript String toUpperCase()](https://www.w3schools.com/jsref/jsref_touppercase.asp)
