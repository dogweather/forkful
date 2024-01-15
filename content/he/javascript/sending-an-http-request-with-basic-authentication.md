---
title:                "שליחת בקשת http עם אימות בסיסי"
html_title:           "Javascript: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## למה

בשפה המתוכנתת Javascript, ישנן מקרים בהם נדרש לשלוח בקשת HTTP עם אימות בסיסי. זה היכולת להתחבר לשרת עם שם משתמש וסיסמה כדי לבצע פעולות או גישה למידע שמוגבל רק למשתמשים מורשים.

במאמר זה, נלמד כיצד לשלוח בקשת HTTP עם אימות בסיסי באמצעות קוד Javascript, כך שתוכלו להשתמש בכך ביכולתיות שלכם כמתכנתי Javascript.

## איך לעשות זאת

כדי לשלוח בקשת HTTP עם אימות בסיסי, נדרשת בקשת AJAX ממשתמש. נתחיל ביצירת אינסטנס חדש לאובייקט XMLHttpRequest עם הקוד הבא:

```javascript
let xhr = new XMLHttpRequest();
```

לאחר מכן, נגדיר את אופן הבקשה באמצעות הפונקציה "open", כאשר נכתוב את המכתבת (GET / POST), את הכתובת המתאימה ואת האופציות לאימות בסיסי. לדוגמה:

```javascript
xhr.open('GET', 'https://example.com', true, 'username', 'password');
```

לבסוף, נשלח את הבקשה עם הפונקציה "send" ונקבל את התשובה בתור טקסט פשוט:

```javascript
xhr.send();
console.log(xhr.responseText);
```

כך אנו שולחים בקשת HTTP עם אימות בסיסי ומקבלים את התוצאה.

## חפירה עמוקה

כפי שראינו בדוגמה, ניתן לשלוח בקשת HTTP עם אימות בסיסי באמצעות חיבור מקשד של AJAX. כמו כן, ניתן להשתמש בגישת Authorization בשביל כתובת האתר לאחר שהכנסת פרטי המשתמש לבקשה. גישת Authorization מאפשרת לכם לקבל גישה למידע מוגבל למשתמשים מסוימים על ידי הכנסת ביטוי סודי בכותרת הבקשה. לדוגמה:

```javascript
xhr.setRequestHeader('Authorization', 'Basic ' + btoa('username:password'));
```

את הכותרת Authorization אתם יכולים להשתמש בה בכל בקשה מ