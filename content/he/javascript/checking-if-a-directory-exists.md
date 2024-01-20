---
title:                "בדיקה אם ספרייה קיימת"
html_title:           "Elixir: בדיקה אם ספרייה קיימת"
simple_title:         "בדיקה אם ספרייה קיימת"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## מה ולמה?

בעת כתיבת קוד תכנות ב-Javascript, יתכן ונצטרך לבדוק האם ספרייה מסוימת קיימת. זה מתבצע כדי למנוע שגיאות בעת ניסיון לגשת לקבצים או ספריות שאינם קיימים.

## איך לעשות:

אנו משתמשים באובייקט `fs` של Node.js:

```Javascript
const fs = require('fs');

if (fs.existsSync('/your/path')) {
    console.log('The directory exists!');
} else {
    console.log('The directory does not exist.');
}
```

כאשר מסבירים את הקוד מסויים, הוא פשוט יחזיר "הספרייה קיימת!" אם הנתיב קיים, ואחרת הוא יחזיר "הספרייה לא קיימת".

## צלילה עמוקה:

הגישה שהצגנו כאן היא של Node.js, פלטפורמה שנוצרה ב-2009, שמאפשרת לנו לבצע תכנות משורת הפקודה. יתכנו אלטרנטיבות אחרות, אבל זוהי הגישה מועדפת ביותר כאשר מדובר ב-Javascript.

מתחת לשורה, `fs.existsSync` מפעילה את הקוד המקומי של המערכת שמשתמשת ב-Node.js לבדוק האם הנתיב קיים - זה לא חלק מהתכנית שלך, אלא מהמערכת המקומית שלך.

## ראה גם:

- [הדוקומנטציה של Node.js על fs.existsSync](https://nodejs.org/api/fs.html#fs_fs_existssync_path)
- [מדריך מדוברקס](https://dev.to/zhiwehu/node-js-check-if-a-path-exists-1joj)
- [Stack Overflow: איך לבדוק האם ספרייה / קובץ קיים עם Node.js](https://stackoverflow.com/questions/4482686/check-synchronously-if-file-directory-exists-in-node-js)