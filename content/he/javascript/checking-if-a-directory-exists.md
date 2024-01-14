---
title:                "Javascript: בדיקת קיום תיקייה"
simple_title:         "בדיקת קיום תיקייה"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## למה
בתחום התכנות, בעת כתיבת קוד, חשוב לוודא שמשתמשי התוכנה שלך יוכלו לגשת לקבצים ותיקיות הנדרשים עבור התוכנה. ודאות כי אתה מגשים תנאי אלו, המקרים בהם יש צורך לבדוק האם תיקייה קיימת במערכת יכולים להיות נפוצים.

## איך לעשות זאת
כדי לבדוק אם תיקייה קיימת במערכת, ניתן להשתמש בפונקציית `fs.existsSync()` שמתבצעת על ידי הכנסת נתיב לתיקייה כפרמטר. ערכו הנובע מכך יחזיר בחזרה ערך בוליאני של `true` אם התיקייה קיימת, ובשלילה ערך של `false` אם התיקייה לא קיימת.

```Javascript
const fs = require('fs');

// בדיקה של תיקייה קיימת
if (fs.existsSync('./תיקייה')) {
    console.log("התיקייה קיימת במערכת.");
} else {
    console.log("התיקייה אינה קיימת במערכת.");
}

// בדיקה של תיקייה לא קיימת
if (fs.existsSync('./תיקייה_לא_קיימת')) {
    console.log("התיקייה קיימת במערכת.");
} else {
    console.log("התיקייה אינה קיימת במערכת.");
}
```
הפלט של הקוד הנ"ל יהיה:

```Javascript
התיקייה קיימת במערכת.
התיקייה אינה קיימת במערכת.
```

## חקירה עמוקה
פונקציית `fs.existsSync()` משתמשת בספריית המערכת הבסיסית של נוד כדי לבדוק אם ישנה תיקייה קיימת בנתיב שנתון. במקרים בהם יש צורך לבדוק רבות תיקיות, מומלץ להשתמש בשרת הדפוס במקום לבצע בדיקה מחדש בכל פעם.

## ראה גם
- [פונקציית `fs.existsSync()` בנוד דוקומנטציה](https://nodejs.org/api/fs.html#fs_fs_existssync_path)
- [דוגמאות עם פונקציית `fs.existsSync()`](https://www.w3schools