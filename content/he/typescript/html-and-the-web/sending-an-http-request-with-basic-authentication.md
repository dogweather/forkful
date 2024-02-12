---
title:                "שליחת בקשת HTTP עם אימות בסיסי"
aliases: - /he/typescript/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:03:28.168328-07:00
model:                 gpt-4-1106-preview
simple_title:         "שליחת בקשת HTTP עם אימות בסיסי"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## מה ולמה?
שליחת בקשת HTTP עם אוטנטיקציה בסיסית היא תהליך שבו משתמשים מצרפים שם משתמש וסיסמא בצורת `Base64` לבקשה כדי לאמת זהות. תוכניתנים עושים זאת כדי ליצור חיבור מאובטח לשרתים הדורשים אימות פשוט.

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
