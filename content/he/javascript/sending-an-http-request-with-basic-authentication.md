---
title:                "Javascript: שליחת בקשת HTTP עם אימות בסיסי"
simple_title:         "שליחת בקשת HTTP עם אימות בסיסי"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# למה

בתכנות ג'אווהסקריפט, ישנם מקרים בהם נדרש לשלוח בקשת HTTP עם אימות בסיסי. זהו תהליך נפוץ בעידן האינטרנט הנוכחי, ובמאמר זה נלמד כיצד לבצע פעולה זאת.

## כיצד לעשות זאת

שליחת בקשת HTTP עם אימות בסיסי בתכנות ג'אווהסקריפט אפשרית באמצעות פונקציות של חבילת HTTP. ננסה לשלוח בקשה עם אימות בסיסי לאתר כלשהו, לדוגמה:

```javascript
// ייבוא חבילת HTTP
const http = require('http');

// נגדיר את פרטי האימות - שם משתמש וסיסמה
const username = 'myUsername';
const password = 'myPassword';

// נבנה את התכנית של הבקשה המכילה את הפרטים עם האימות
const options = {
  hostname: 'www.example.com', // שם האתר שנשלח אליו הבקשה
  port: 80, // פורט המאפשר גישה לאתר
  path: '/', // הנתיב של העמוד הראשי של האתר
  method: 'GET', // סוג הבקשה - לדוגמה: GET, POST, PUT, DELETE
  headers: { // כותרות נוספות שניתן לכלול
    Authorization: 'Basic ' + Buffer.from(username + ':' + password).toString('base64') // אימות בסיסי
  }
};

// נשלח את הבקשה לאתר ונקבל תשובה
const req = http.request(options, (res) => {
  console.log(`קוד מצב התגובה: ${res.statusCode}`); // קוד מצב התגובה של האתר
  // הצגת התוכן של התגובה
  res.on('data', (d) => {
    process.stdout.write(d);
  });
});

// אם יש טעות בשליחת הבקשה
req.on('error', (error) => {
  console.error(error);
});

req.end();
```

הפלט של הקוד הנ"ל יוצא לדוגמה כך:

```
קוד מצב התגובה: 200
<!doctype html>
<html>
...
```

נשים לב שבהגדרת הכותרת בבקשה, נציג את האימות הבסיסי באופן נכון על ידי המרת השם משתמש והסיסמה למבנה של Base64. בכוונתיות הוגדר הכתובת של ה