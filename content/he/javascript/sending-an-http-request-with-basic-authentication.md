---
title:                "שליחת בקשת http עם אימות בסיסי"
html_title:           "C: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## מה זה ולמה? 

שליחת בקשת HTTP עם אימות בסיסי הוא הרבה יותר מהמילים שהובאו כאן. זו התהליך שבו אנחנו מנהלים ביטחונות של משתמש על השרת. המתכנתים עושים את זה להבטיח שרק המשתמשים המאומתים יכולים להתחבר ולגשת למשאבים.

## איך לבצע: 

אז כיצד אתה שולח בקשת HTTP עם אימות בסיסי? הנה דוגמה ב-js:

```Javascript
const https = require('https');

const options = {
    hostname: 'www.example.com',
    port: 443,
    path: '/api',
    method: 'GET',
    auth: 'username:password'
};

const req = https.request(options, res => {
    res.on('data', d => {
        process.stdout.write(d);
    });
});

req.on('error', error => {
    console.error(error);
});

req.end();
```

הדוגמה מראה את היכולת של `node.js` לשנות את אות הבקשה הנשלחת לשרת על ידי שליחת שם משתמש וסיסמא מאומתים בכותרת האימות.


## צלילה עמוקה: 

אימות בסיסי הוא שיטה ישנה ומנומשת, אך טיבעית של אימות ב-HTTP, ששרתים רבים ולקוחות יכולים לתמוך בקלות. בראש ובראשונה, השיטה אינה מאובטחת מאוד, מכיוון שהמידע משודר בצורה לא מוצפנת. אם אתה מעוניין באימות מאובטח יותר, בחן אפשרויות כמו אימות טוקן בעזרת JWT.

אם אתה רוצה לשנות את הבקשה שיש לך לשרת, כמו לדוגמה `options`, אתה יכול לעשות זאת בעזרת ה-`auth` property. אתה רק צריך להזין את השם והסיסמה שלך, מופרדים על ידי נקודתיים (`:`).

##ראה גם:

אם אתה רוצה להתחיל לעבוד עם הרבה המידע שהצגתי לך כאן, אני ממליץ לך להסתכל על כמה משאבים הבאים:

1. [מידע נוסף על אימות בסיסי ב-HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
2. [מדריך מקיף לאימות בJavaScript ו-Node.js](https://www.toptal.com/express-js/nodejs-typescript-rest-api-pt-1)