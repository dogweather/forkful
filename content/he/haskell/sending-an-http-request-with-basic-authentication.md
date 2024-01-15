---
title:                "שליחת בקשת http עם אימות בסיסי"
html_title:           "Haskell: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## למה

כשמדובר בפיתוח פרויקטים שכולם יכולים להיכנס אליהם, כמו אתרי אינטרנט או ממשקי API, יש צורך למנות בפני ישראל במנגנון אימות משתמש. אחד המנגנונים הכי פשוטים ונפוצים הוא האימות הבסיסי באמצעות שליחת בקשת HTTP עם פרטי הכניסה בשדות ההפעלה.

## איך לעשות

התחל על ידי התקנת החבילה "http-request" באמצעות הפקודה הבאה בטרמינל:

```Haskell
cabal install http-request
```

לאחר מכן, ייבא את המודול "Network.HTTP.Simple" בעזרת הפקודה הבאה:

```Haskell
import Network.HTTP.Simple
```

כעת, ניתן ליצור את הבקשה הרלוונטית ולשלוחה כפי שמפורט בקוד הבא. אנא ודא שהכנסת את הפרטים הנכונים בתוך הפונקציה "setRequestBasicAuth":

```Haskell
exampleRequest :: IO ()
exampleRequest = do
  request <- parseRequest "https://example.com/"
  let request' = setRequestBasicAuth "username" "password" request
  response <- httpLBS request'
  print $ getResponseBody response
```

תוצאת פלט היא גוף התגובה של הבקשה המכיל את כל התוכן של האתר המבוקש.

## מעמיקים

כאשר אתה שולח בקשת HTTP עם אימות בסיסי, הבקשה מכילה שדות הפעלה שמכילים את שם המשתמש והסיסמה בפורמט "שם משתמש:סיסמה". יש מספר תקנים ונושאים נוספים שכדאי לדעת בעת שליחת בקשת HTTP עם אימות בסיסי, כגון בצורת הסיסמה או כותרת "Authorization" בפרוטוקול ה-HTTP.

## ראה גם

- [מדריך לשליחת בקשות HTTP באמצעות Haskell](https://haskell-lang.org/tutorial/http-requests)
- [מסמך רשמי על פרוטוקול HTTP](https://tools.ietf.org/html/rfc7235)