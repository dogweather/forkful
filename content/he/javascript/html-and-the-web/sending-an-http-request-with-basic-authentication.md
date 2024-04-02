---
date: 2024-01-20 18:02:07.934587-07:00
description: "\u05D1\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05E2\
  \u05DD \u05D0\u05D9\u05DE\u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9, \u05D0\u05E0\
  \u05D7\u05E0\u05D5 \u05DE\u05E6\u05E8\u05E4\u05D9\u05DD \u05E4\u05E8\u05D8\u05D9\
  \ \u05D4\u05D6\u05D3\u05D4\u05D5\u05EA (\u05E9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\
  \ \u05D5\u05E1\u05D9\u05E1\u05DE\u05D4) \u05DC\u05D1\u05E7\u05E9\u05D4. \u05DE\u05EA\
  \u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\
  \u05D4 \u05DB\u05D3\u05D9 \u05DC\u05D0\u05D1\u05D8\u05D7 \u05D2\u05D9\u05E9\u05D4\
  \ \u05DC\u05DE\u05E9\u05D0\u05D1\u05D9 \u05E8\u05E9\u05EA \u05DE\u05D5\u05D2\u05D1\
  \u05DC\u05D9\u05DD."
lastmod: '2024-03-13T22:44:39.974232-06:00'
model: gpt-4-1106-preview
summary: "\u05D1\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05E2\
  \u05DD \u05D0\u05D9\u05DE\u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9, \u05D0\u05E0\
  \u05D7\u05E0\u05D5 \u05DE\u05E6\u05E8\u05E4\u05D9\u05DD \u05E4\u05E8\u05D8\u05D9\
  \ \u05D4\u05D6\u05D3\u05D4\u05D5\u05EA (\u05E9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\
  \ \u05D5\u05E1\u05D9\u05E1\u05DE\u05D4) \u05DC\u05D1\u05E7\u05E9\u05D4. \u05DE\u05EA\
  \u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\
  \u05D4 \u05DB\u05D3\u05D9 \u05DC\u05D0\u05D1\u05D8\u05D7 \u05D2\u05D9\u05E9\u05D4\
  \ \u05DC\u05DE\u05E9\u05D0\u05D1\u05D9 \u05E8\u05E9\u05EA \u05DE\u05D5\u05D2\u05D1\
  \u05DC\u05D9\u05DD."
title: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05E2\u05DD\
  \ \u05D0\u05D9\u05DE\u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9"
weight: 45
---

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
