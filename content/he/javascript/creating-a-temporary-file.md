---
title:                "Javascript: יצירת קובץ זמני"
simple_title:         "יצירת קובץ זמני"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## למה

יצירת קבצים זמניים היא כלי חשוב בפיתוח תוכנה ותחזוקתה. זה מאפשר למתכנתים ליצור מידע מתוך קוד מזדמן ולהשתמש בו כדי לבדוק רעיונות ולבנות פתרונות.

## כיצד לעשות את זה

```Javascript

// צור קובץ זמני בשם "temp.txt"
var fs = require('fs');
fs.write('temp.txt','Hello World', (err) => {
    if (err) throw err;
    console.log('Temporary file created');
});

// קרא והדפס את תוכן הקובץ הזמני
fs.readFile('temp.txt', 'utf8', (err, data) => {
  if (err) throw err;
  console.log(data);
});

// מחק את הקובץ הזמני
fs.unlink('temp.txt', (err) => {
  if (err) throw err;
  console.log('Temporary file deleted');
});
```

**פלט:**

Temporary file created
Hello World
Temporary file deleted

## חקירה מעמיקה

יצירת קבצים זמניים משתמשת במנגנון המורכב ביותר ומיועדת להשתמש במקרים בהם יש צורך ליצור ולמחוק קבצים בזמן ריצה. השימוש בפעולות מנהל מערכת ליצירת קבצים זמניים מאפשר למתכנתים להפעיל קוד באופן יעיל ובטוח, מבלי לפגוע במערכת הקיימת.

## ראה גם

- [מנהל הקבצים של Node.js ויצירת קבצים זמניים](https://nodejs.org/api/fs.html#fs_file_system_and_path)
- [מדריך תוכנות קבצים זמניים ב-Javascript](https://flaviocopes.com/javascript-temporary-files/)
- [מאמר של קודהאס על פעולות מנהל הקבצים ב-Javascript](https://www.codahale.com/how-to-create-a-temporary-file-and-directory-in-node-js/)