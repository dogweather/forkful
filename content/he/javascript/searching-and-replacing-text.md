---
title:                "חיפוש והחלפת טקסט"
html_title:           "Elm: חיפוש והחלפת טקסט"
simple_title:         "חיפוש והחלפת טקסט"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## מה ולמה? 

חיפוש והחלפה של טקסט הוא ההתאמה של מחרוזת לתבנית מיוחדת והחלפתה במחרוזת חדשה. מתכנתים עושים את זה כדי לשנות, לתקן או לשדרג קוד באופן מהיר ויעיל.

## איך?

בלוק הקוד הבא מציג את החיפוש וההחלפה של טקסט באמצעות JavaScript:

```Javascript
let str = "שלום עולם!";
let newStr = str.replace("עולם", "javascript");
console.log(newStr);
```

התוצאה שתודפס היא: `"שלום javascript!"`

## צלילה עמוקה

(1) במקור, פונקציית ה-replace ב-JavaScript יכלה להחליף רק מופע ראשון של מחרוזת. בעזרת ES6, מתאגרת מחזוריות משופרת הוצגה, מאפשרת את ההחלפה של כל המופעים של המחרוזת.
(2) אלטרנטיבות שונות כוללות שימוש ב-loop לטיפול במחרוזת או ביישום של מחלק אלגוריתמים מבני נתונים.
(3) אפשר להשתמש בתו גלובלי 'g' בשילוב עם regex להחלפה גלובלית של המחרוזת:

```Javascript
let str = "שלום עולם! עולם, טוב לראות אותך!";
let newStr = str.replace(/עולם/g, "javascript");
console.log(newStr);
```
זה ידפיס: `"שלום javascript! javascript, טוב לראות אותך!"`

## ראה גם 

1. [MDN replace() method](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
2. [JavaScript RegExp](https://www.w3schools.com/js/js_regexp.asp)
3. [Understanding JavaScript's replace()](https://www.digitalocean.com/community/tutorials/js-string-replace-method)