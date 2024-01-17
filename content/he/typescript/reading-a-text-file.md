---
title:                "קריאת קובץ טקסט"
html_title:           "TypeScript: קריאת קובץ טקסט"
simple_title:         "קריאת קובץ טקסט"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## מה זה ולמה?

קריאת קובץ טקסט היא פעולה שמבקשת מהמחשב לקרוא ולהציג תוכן מסוים שנמצא בקובץ. זהו פעולה חשובה בתכנות כי היא מאפשרת לנו לקבל מידע מסוגים שונים ולהשתמש בו בתוכניות שלנו.

## איך לעשות זאת:

מטרת הדוגמאות הבאות היא להראות לך כיצד לקרוא קובץ טקסט ב TypeScript ולהציג את התוכן שלו:

```TypeScript
const fs = require('fs');

// קריאת קובץ טקסט 
const content: string = fs.readFileSync('file.txt', 'utf8');

// הצגת התוכן שנמצא בקובץ
console.log(content);

```

תהליך זה נפוץ בעיגולים של תכנות כי הוא מאפשר לנו לגשת למידע ולהשתמש בו בהמשך בתוכניות שלנו.

## ים עמוק

מעניין לדעת שהתהליך של קריאת קובץ טקסט הוא פעולה ישנה שמשמשת את המחשבים כבר כמה עשורים. עם זאת, ייתכן שתרגיש שהדרך שהראינו לך לעשות זאת היא מסורבלת ואינה נחיה כמו שצריך. במקרה כזה, תוכל לבחור להשתמש בספריית גרסת Node.js שנקראת `fs-extra`, המספקת פונקציות נוחות יותר לקריאת וכתיבת קבצים טקסט.

כמו כן, אם התוכניות שלך כוללות עבודה עם קבצים מסוגים נוספים (למשל קבצי Excel או PDF), תוכל להשתמש בספריות נוספות כמו `xlsx` או `pdfkit` שיאפשרו לך לקרוא ולהציג את התוכן שלהם.

## ראה גם

* [ספריית fs-extra של Node.js](https://github.com/jprichardson/node-fs-extra)
* [ספריית xlsx](https://github.com/SheetJS/js-xlsx)
* [ספריית pdfkit](https://github.com/foliojs/pdfkit)