---
date: 2024-01-20 18:03:28.168328-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D0\u05D5\u05D8\
  \u05E0\u05D8\u05D9\u05E7\u05E6\u05D9\u05D4 \u05D1\u05E1\u05D9\u05E1\u05D9\u05EA\
  \ \u05D1-HTTP \u05D4\u05D9\u05D0 \u05E4\u05E9\u05D5\u05D8\u05D4 \u05D5\u05D9\u05E9\
  \u05D9\u05E8\u05D4, \u05D0\u05DA \u05DC\u05D0 \u05E0\u05D7\u05E9\u05D1\u05EA \u05DC\
  \u05D1\u05D8\u05D5\u05D7\u05D4 \u05D1\u05DE\u05D9\u05D5\u05D7\u05D3 \u05DB\u05D9\
  \ \u05D4\u05D0\u05D9\u05E0\u05E4\u05D5\u05E8\u05DE\u05E6\u05D9\u05D4 \u05E0\u05E9\
  \u05DC\u05D7\u05EA \u05DB\u05D8\u05E7\u05E1\u05D8 \u05E4\u05E9\u05D5\u05D8. \u05D1\
  \u05E2\u05D1\u05E8, \u05D4\u05D9\u05D0 \u05D4\u05D9\u05D9\u05EA\u05D4 \u05D3\u05E8\
  \u05DA \u05E0\u05E4\u05D5\u05E6\u05D4 \u05DC\u05D0\u05D9\u05DE\u05D5\u05EA, \u05D0\
  \u05D1\u05DC\u2026"
lastmod: '2024-04-05T22:50:53.161449-06:00'
model: gpt-4-1106-preview
summary: "\u05D0\u05D5\u05D8\u05E0\u05D8\u05D9\u05E7\u05E6\u05D9\u05D4 \u05D1\u05E1\
  \u05D9\u05E1\u05D9\u05EA \u05D1-HTTP \u05D4\u05D9\u05D0 \u05E4\u05E9\u05D5\u05D8\
  \u05D4 \u05D5\u05D9\u05E9\u05D9\u05E8\u05D4, \u05D0\u05DA \u05DC\u05D0 \u05E0\u05D7\
  \u05E9\u05D1\u05EA \u05DC\u05D1\u05D8\u05D5\u05D7\u05D4 \u05D1\u05DE\u05D9\u05D5\
  \u05D7\u05D3 \u05DB\u05D9 \u05D4\u05D0\u05D9\u05E0\u05E4\u05D5\u05E8\u05DE\u05E6\
  \u05D9\u05D4 \u05E0\u05E9\u05DC\u05D7\u05EA \u05DB\u05D8\u05E7\u05E1\u05D8 \u05E4\
  \u05E9\u05D5\u05D8."
title: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05E2\u05DD\
  \ \u05D0\u05D9\u05DE\u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9"
weight: 45
---

## איך לעשות:
```TypeScript
import axios from 'axios';

const getProtectedData = async () => {
  try {
    const username = 'yourUsername';
    const password = 'yourPassword';
    const token = Buffer.from(`${username}:${password}`).toString('base64');

    const response = await axios.get('https://your-protected-resource.com', {
      headers: {
        'Authorization': `Basic ${token}`
      }
    });
    
    console.log(response.data);
  } catch (error) {
    console.error(error);
  }
};

getProtectedData();
```
דוגמת פלט:
```
{ "protected": "data" }
```

## עיון מעמיק
אוטנטיקציה בסיסית ב-HTTP היא פשוטה וישירה, אך לא נחשבת לבטוחה במיוחד כי האינפורמציה נשלחת כטקסט פשוט. בעבר, היא הייתה דרך נפוצה לאימות, אבל היום היא לעיתים נחשבת לפרומיטיבית ונעדפות שיטות אימות מתקדמות יותר כמו OAuth. חשוב להשתמש ב-HTTPS כדי לאבטח בקשות עם אוטנטיקציה בסיסית. השימוש בקודירת `Base64` אינו מצפין את הנתונים, אלא רק מקודד אותם בצורה קריאה.

## ראו גם
- [Axios GitHub repository](https://github.com/axios/axios) – לספריית HTTP לקוח שמשמשת בדוגמה.
- [HTTP Basic Authentication - MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#basic_authentication_scheme) – למדו על אוטנטיקציה בסיסית ב-HTTP באתר MDN.
- [Buffer על נוד ג'ס API](https://nodejs.org/api/buffer.html) – למידע על מחלקת Buffer ב-Node.js, שמשמשת ליצירת קוד `Base64`.
- [Understanding Base64 Data Encoding](https://www.base64encode.org/) – להבנה עמוקה יותר של הקידוד `Base64`.
