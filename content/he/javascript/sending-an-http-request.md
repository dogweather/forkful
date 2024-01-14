---
title:                "Javascript: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

שלום וברוכים הבאים לפוסט פרוגרמינג בסיסי ב-Javascript! היום נדבר על כיצד לשלוח בקשת HTTP ב-Javascript ולמה יכולה להיות צורה שימושית לעשות זאת.

##למה

שליחת בקשת HTTP ב-Javascript יכולה להיות שימושית כאשר אנחנו רוצים ליצור תקשורת בין הדפים שלנו לבין שרת חיצוני. כך נוכל לקבל נתונים חדשים, לשלוח פרמטרים ולקבל תגובות מהשרת בזמן אמת.

##כיצד לעשות זאת

כדי לשלוח בקשת HTTP ב-Javascript, נשתמש בפונקציות הקיימות בעזרת כתב קוד קצר ופשוט. לדוגמה, כדי לשלוח בקשה GET, נשתמש בפונקציה "fetch" ונכניס את הכתובת של השרת ואת המידע הרלוונטי כארגומנטים. בקוד השלוחה ניתן לקבל גם נתונים מהשרת ולעבד אותם בהתאם.

```Javascript
fetch('https://example.com/data')
.then(response => response.json())
.then(data => console.log(data))
.catch(err => console.log(err));
```

במשך זמן אמת, ניתן לשלוח בקשות נוספות ולקבל תגובות מהשרת, וכך ליצור תקשורת חלקה ויעילה בין הדפים והשרת.

##להתפנק

שליחת בקשת HTTP ב-Javascript מאפשרת לנו גם לקבל מידע מתקדם על השרת ועל מענה התקשורת. ניתן להשתמש בכלים נוספים כמו תעודות אבטחת SSL, הגדרות איסוף מידע והגבלות מסוימות על מגבלות הבקשה. כמו כן, באמצעות כלים נוספים ניתן לנהל ולעקוב אחרי בקשות שנשלחות ולאחר את הנתונים שנתקבלו מהשרת.

##ראה גם

* [MDN על שליחת בקשת HTTP עם Javascript](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch)
* [סרטון על השתמש