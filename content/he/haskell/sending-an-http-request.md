---
title:                "שליחת בקשת http."
html_title:           "Haskell: שליחת בקשת http."
simple_title:         "שליחת בקשת http."
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## למה
כרגע, שליחת בקשת HTTP היא חלק בלתי נפרד מהדרכון בתחום התכנות. היא משמשת כדי ליצור תקשורת בין שתי מערכות ולקבל מידע בצורה מהירה ויעילה.

## איך לעשות זאת
לשלוח בקשת HTTP ב- Haskell ישנם מספר ביטויים שיכולים להיות מועילים:

```Haskell
import Network.HTTP.Simple

-- יצירת בקשת GET פשוטה לאתר
response <- httpGetRequest "https://example.com"

-- שימוש במתודה יחידה כדי לקבל מידע מפורמט JSON
response <- httpJSONRequest "https://example.com/api/users"

-- בדיקה של מצב התקשורת שלנו
alive <- isAlive "https://example.com"
```

* ```httpSimpleRequest``` מאפשר לנו לשלוח בקשות פשוטות ולקבל תגובה בצורה נוחה ומבורכת.
* ```httpJSONRequest``` מאפשר לנו גם לשלוח בקשות ולקבל מידע מפורמט JSON שנוח לעבוד איתו.
* ```isAlive``` מאפשר לנו לבדוק את מצב התקשורת שלנו ולהבין האם האתר לא נגיש על ידי המחשב שלנו.

## עומק בנושא
כאשר אנחנו שולחים בקשת HTTP, ישנם מספר שלבים שמתרחשים מאחורי הקלעים:

* יצירת חיבור HTTP עם השרת שאנחנו מבקשים ממנו מידע.
* שליחת הבקשה בצורה של HTTP וקבלת התגובה לחזרה.
* עיבוד התוצאה המתקבלת והצגתה למשתמש.

כאשר אנחנו עובדים ב-Haskell ישנם ספריות וכלים שיכולים לסייע לנו בביצוע החלקים השונים של שליחת בקשת HTTP ועיבוד המידע המתקבל.

## ראה גם
* [הרשמה לסדרת כתבות על Haskell](https://www.haskell.org/)
* [תיעוד לספריית Network.HTTP.Simple](https://hackage.haskell.org/package/http-client-0.6.4/docs/Network-HTTP-Simple.html)
* [מדריך לשליחת בקשות HTTP ב- Haskell](https://www.haskell.org/haskellwiki/Introduction_to_Haskell_IO/HTTP)