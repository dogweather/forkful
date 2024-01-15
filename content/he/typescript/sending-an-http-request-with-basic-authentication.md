---
title:                "שליחת בקשת HTTP עם אימות בסיסי."
html_title:           "TypeScript: שליחת בקשת HTTP עם אימות בסיסי."
simple_title:         "שליחת בקשת HTTP עם אימות בסיסי."
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## למה

למה מישהו היה רוצה לשלוח בקשת HTTP עם אימות בסיסי? כי כאשר משתמשים ב HTTP אינו מאובטח, שרתים מצפים לקבל מידע הוכחה כדי להתרשם מזהות ולהונות זה באופן אותנטי וחוקי.

## כיצד לעשות זאת

עם TypeScript, זה מאוד פשוט לשלוח בקשת HTTP עם אימות בסיסי. בקוד הטקסט המתחת תוכלו לראות דוגמאות של כיצד לממש את זה בצורה נכונה עם תצוגת תקינה בחזרה מהשרת.

```TypeScript
// ייבוא המודול 
import axios from 'axios';

// הפעלת בקשת HTTP עם אימות בסיסי
const response = await axios.post('http://www.example.com/api', {}, {
  auth: {
    username: 'username',
    password: 'password'
  }
});

// פלט התצוגה בעת הקבלה של תגובה מהשרת
console.log(response.data);
```

### הכוונה לתוצאה חוזרת:

```TypeScript
{
  status: 200,
  message: 'הצלחה'
}
```

## כיוון מקדים

כדי לשלוט מטה HTML, למשל צורך לשלוח לשרת בקשה עם מידע על המשתמש, אנחנו צריכים להישאב עוזר לטכנולוגיות המתאימות לעבר בקשת HTTP עם אימות בסיסי. באמצעות TypeScript, ניתן לבצע זאת בצורה נכונה ובטוחה.

## כיצד זה עובד

כאשר משתמשים ב HTTP אינו מאובטח, השרת צפוי לאפשר גישה לקוד מהצד של המשתמש ממקור חוקי לוגי מהצד הנמלט. כאשר ניאות טבע מצפון תוך הוספת סימן אם התעבורה מטופלת וטובי. פשוט במחשב או URL- הוא נאסף מшודד ישירות.

## רק משום שניתן לעבוד עם TypeScript בצורה אבטחת יותר, אנחנו יכולים לשלוח בקשות HTTP עם אימות בסיסי ולהעביר את נתוני המשתמש בצורה בטוחה