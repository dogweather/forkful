---
title:                "קריאת קובץ טקסט"
html_title:           "Go: קריאת קובץ טקסט"
simple_title:         "קריאת קובץ טקסט"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## מה ולמה?

קריאת קובץ טקסט היא תהליך שבו התוכנית מתקשרת עם המערכת הפעלה על מנת לגשת לנתונים השמורים בקובץ. מתכנתים משתמשים בכך כדי לשמור, לגשת או לעבד נתונים במהלך ריצת התוכנית.

## איך לעשות:

אנחנו בתוך Javascript, חלק מ- Node.js. נשתמש במודול המובנה 'fs'.

```javascript
const fs = require('fs');
  
fs.readFile('example.txt', 'utf8' , (err, data) => {
  if (err) {
    console.error(err);
    return;
  }
  console.log(data);
});
```

הוא מדפיס את כל הטקסט שב- 'example.txt'. 

אם הקובץ לא קיים, זה מדפיס שגיאה.

## צלילה עמוקה:

אסינכרוניות היא השיטה המועדפת ב- Node.js. אבל המודול 'fs' מציע גם שיטה סינכרונית, אם זה מה שאתם צריכים.

```javascript
// קריאה סינכרונית
const data = fs.readFileSync('example.txt', 'utf8');
console.log(data);
```

"ReadFile" כתוב בסטנדרט POSIX, שהוא תקן של פונקציות מערכות הפעלה ערכתיות. זמן הקריאה נשלט אך ורק על ידי מערכת ההפעלה.

## ראה גם:

הדוקומנטציה המלאה של המודול 'fs': [Node.js 'fs' Docs](https://nodejs.org/api/fs.html)
מדריך מעולה לקבצי מערכת ב- Javascript: [Working with Files in JavaScript](https://www.javascripture.com/File)