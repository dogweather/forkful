---
title:                "שליחת בקשת HTTP עם אימות בסיסי"
aliases:
- /he/javascript/sending-an-http-request-with-basic-authentication/
date:                  2024-01-20T18:02:07.934587-07:00
model:                 gpt-4-1106-preview
simple_title:         "שליחת בקשת HTTP עם אימות בסיסי"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## מה ולמה?
בשליחת בקשת HTTP עם אימות בסיסי, אנחנו מצרפים פרטי הזדהות (שם משתמש וסיסמה) לבקשה. מתכנתים עושים את זה כדי לאבטח גישה למשאבי רשת מוגבלים.

## איך לעשות:
קוד לדוגמא ופלט דוגמא:

```Javascript
const axios = require('axios');
const base64 = require('base-64');

// הפרטים שלך לאימות
const username = 'your_username';
const password = 'your_password';

// יצירת ראש HTTP של אימות בסיסי
const basicAuth = 'Basic ' + base64.encode(username + ':' + password);

// ביצוע בקשת HTTP עם אימות בסיסי
axios.get('https://your.api.endpoint/', { headers: { 'Authorization': basicAuth } })
  .then(response => {
    console.log(response.data); // פלט של תגובת השרת
  })
  .catch(error => {
    console.error('Authentication failed:', error);
  });
```

## עיון נוסף
אימות בסיסי בשימוש מאז התחלות האינטרנט. הייתכן שהוא לא הכי בטוח, אך הוא פשוט וישיר. חלופות נפוצות כוללות אימות באמצעות טוקנים כמו JWT (JSON Web Tokens), OAuth, ו-API Keys. כאשר משתמשים באימות בסיסי, חשוב לשקול תמיד שימוש בחיבור מאובטח (HTTPS) כדי להגן על הנתונים המועברים.

## ראה גם
- [MDN Web Docs on HTTP authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [Axios library on GitHub](https://github.com/axios/axios)
- [Base-64 npm package](https://www.npmjs.com/package/base-64)
- [Understanding JWT](https://jwt.io/introduction/)
