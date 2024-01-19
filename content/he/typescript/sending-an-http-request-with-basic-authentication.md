---
title:                "שליחת בקשת http עם אימות בסיסי"
html_title:           "C: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## מה זה ולמה?
שליחת בקשת HTTP עם אימות בסיסי היא שיטה שבה פרטי המשתמשים מועברים בעזרת כותרת `Authorization` של הבקשה. תכנתים משתמשים בזה כדי לאמת משתמשים על מנת לאפשר גישה רק למשאבים מסוימים.

## איך לקודד:
עליך להוסיף את כותרת `Authorization` לבקשת ה-HTTP שלך ולספק את שם המשתמש והסיסמה שלך בתוך הכותרת:

```TypeScript
import axios from 'axios';

async function sendRequest() {
  const username = 'username';
  const password = 'password';
  const encodedCredentials = Buffer.from(`${username}:${password}`).toString('base64');

  const response = await axios.get('https://some-url', { 
    headers: { 
      'Authorization': `Basic ${encodedCredentials}` 
    }
  });

  console.log(response.data);
}

sendRequest();
```

כאשר אתה מפעיל את הקוד שלך, אתה צפוי לראות את התגובה מהשרת בקונסול שלך.

## התרעה אל מוקד:
שליחת בקשת HTTP עם אימות בסיסי היא מסלול שנקבע כפתרון מוקדם לאימות משתמשים ב-Web. בעבר, היא הייתה מכילה את השמות והסיסמאות בצורה של מילה אחת. אף על פי שהפתרון הנוכחי משתמש בקידוד base64, הוא לא מאפשר הצפנה ובכך מסכן את פרטי המשתמש. חלופות לאימות בסיסי כוללים הצפנת SSL / TLS, OAuth ונתבים API בקנה מידה גדול.

## ראה גם:
כדי לקבל מידע נוסף, בדוק את המקורות הבאים:
- [Basic Authentication - MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [Axios - NPM](https://www.npmjs.com/package/axios)
- [שליחת בקשת HTTP עם אימות בסיסי - Stack Overflow](https://stackoverflow.com/questions/34558264/fetching-data-with-basic-auth-and-http-get)