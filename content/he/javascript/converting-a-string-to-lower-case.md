---
title:                "Javascript: המרת מחרוזת לאותיות קטנות"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why
כתיבת קוד נכון היא מפתח להצלחה בתחום התכנות. כאשר מפתחים גוגל מזינים את הקוד שלנו, שם הקוד קשה לפענח מאחר ורשומה לא תמיד נראית בצורה תקינה. על מנת לשפר את זה, חשוב להעביר את הטקסט לאותיות קטנות בפתרון הקוד. כגון, להפוך את "HELLO" ל"hello". הדגש על זה נמצא בעיבוד תוויות משום שאותיות גדולות וקטנות מייצגות ערכים שונים בתכנות וסדריות.

## How To
```Javascript
// פונקציה שממירה טקסט לאותיות קטנות
function toLowercase(text) {
    return text.toLowerCase();
}
// מקרה נבחר: המרת המחרוזת "HELLO" לקרטה
let convertedText = toLowercase("HELLO"); 

console.log(convertedText); // output: "hello"
```
כאן, אנו משתמשים בפונקציה מובנית של JavaScript - `toLowerCase()` - להמרת התווים לאותיות קטנות. הפונקציה מקבלת את הטקסט כפרמטר ומחזירה את הטקסט המומר באותיות קטנות. זהו פתרון פשוט ויעיל למטרה שלנו.

## Deep Dive
ההמרה לאותיות קטנות בתכנות יכולה להיות מורכבת יותר ממה שנראה במבט ראשון. לדוגמה, כאשר אנו משתמשים בפונקציה `toLowerCase()` למחרוזת המכילה תווים בלתי אותיותיים, היא לא תשנה את התווים האלה. לדוגמה: `toLowerCase("123")` יחזיר עדיין "123". בנוסף, הפונקציה לא משנה את הטווח של התווים. זה פחות ניכר כשמתעסקים עם אותיות גדולות וקטנות בלבד, אבל כאשר מתחילים לשנות את גודל הטווחים, זה חשוב לזכור.

## See Also
[MDN פונקציית toLowerCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference