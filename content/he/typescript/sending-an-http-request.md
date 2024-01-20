---
title:                "שליחת בקשת http"
html_title:           "Bash: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## מה ולמה?
שליחת בקשה HTTP היא בראש ובראשונה תקשורת בין שני מחשבים דרך האינטרנט:  אחד ששולח (Client) והשני שמקבל (Server). מהו המטרה? נפוח מאד, בין היתר לקריאת נתונים, שינוי נתונים וביצוע פקודות/פעולות בצד השרת.

## איך לכתוב:
ראשית, התקנו את החבילה axios באמצעות הפקודה `npm install axios`. עכשיו נוכל לעשות בקשה HTTP:

```TypeScript
import axios from 'axios';

axios.get('https://api.example.com/data')
  .then(response => {
    console.log(response.data);
  })
  .catch(error => {
    console.error(error);
  });
```

הפלט של הקוד הזה יהיה הנתונים שהשרת מחזיר בתגובה לבקשה שלנו.

## צלילה מעמיקה:
בעבר, נעשה שימוש רחב של חבילת XMLHttpRequest על מנת לבצע בקשות HTTP. אך באנשי הדור החדש מעדיפים לשלוח בקשות HTTP באמצעות חבילת axios שתומכת Promise באופן מובנה ומאפשרת שליחה קלה ונוחה של בקשות HTTP.

חלונית שימשה ב-HTTP 2.0 כגרסה הראשונה שדילתה עם נושאים של אילתור נתונים ותקשורת יעילה בין שרת ולקוח.

## ראו גם:
1. מזהים ל-APIים מרשת על מנת לבדוק בקשות HTTP - https://www.postman.com/
2. מידע נוסף לגבי בקשות HTTP בצד השרת - [Server-side Requests with Node.js](https://nodejs.dev/learn/making-http-requests-with-nodejs)
3. עוד מדריך פופולרי לבקשות HTTP בעזרת TypeScript - [An Introduction to TypeScript: Static Typing for the Web](https://www.sitepoint.com/an-introduction-to-typescript/)