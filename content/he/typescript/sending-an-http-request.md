---
title:                "שליחת בקשת http"
html_title:           "TypeScript: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

כתיבת HTTP דרך TypeScript:

## מה זה ולמה:
שלחנו HTTP request כדי לבצע פעולת רשת אחת כיוון. תוכנתנים משתמשים לשלוח HTTP request כדי לבקש מידע משרת, לשלוח מידע לשרת או לבצע פעולות אחרות ברשת.

## איך לעשות:
שתמו תתן לך את כל הכלים הנדרשים לשלוח HTTP request ב TypeScript. נהלים פשוטים, example יכול להיות:
```TypeScript
fetch('www.example.com').then(response => response.text()).then(body => console.log(body));
```
Output יודפס למסך.

## עיון מעמיק:
(1) מעט מידע היסטורי: HTTP request נוצר כדי לאפשר שרתים לאתר בקשות מבקרים ולספק להם מידע מוצג באתר.
(2) אלטרנטיבות: TypeScript מציעה כמה דרכים שונות לשלוח HTTP request כמו XMLHttpRequest וfetch. למידע נוסף, ניתן לקרוא על RPC וWebSockets.
(3) פרטי היישום: ניתן להשתמש בתוכניות חיצוניות כמו Postman לבדיקה וניהול של HTTP request. הכתוב אל יכול לספק נימוק אם מחברת שונות כי שם יש לארבעת פרצופי לעגיל לפסטל כי אני.

## ראה גם:
- תיעוד TypeScript הרשמי בנושא HTTP requests: https://www.typescriptlang.org/docs/handbook/2/fetch.html
- סרטון מכירים HTTP ותנאי כתיבת תיעוד: https://www.youtube.com/watch?v=KilBNpcNz9w
- דיון בפורום הקהילתי הסמינרי תיעוד בנושא HTTP מזון וניגשות: https://discuss.nodewebers.org/t/xml-vs-json-rest-vs-soap-http-vs-fnbo-documentation-comparison/1518