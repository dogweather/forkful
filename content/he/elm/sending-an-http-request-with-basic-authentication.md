---
title:                "Elm: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## למה
לפני שנכנסים לדרך הטכנית של שליחת בקשת HTTP עם אימות בסיסי, חשוב להבין את המטרה של התהליך. שליחת בקשת HTTP עם אימות בסיסי היינו כדי לבצע פעולות אימות, כגון כניסה לאתר או גישה למידע המוגן באמצעות אפליקציות חיצוניות.

## איך לעשות זאת
```Elm
sendBasicAuthRequest : String -> Html msg
sendBasicAuthRequest url =
let
username = "myUsername"
password = "myPassword"
headers =
[ ( "Authorization", "Basic " ++ username ++ ":" ++ password )
]
request =
Http.get url
|> Http.addHeaders headers
in
    Html.div []
        [ Html.text "Request Sent!" ]
```

```
Output:
Request Sent!
```

המשתמש נדרש להכניס את שם המשתמש והסיסמה שלו בשורת הקוד, ובכך נוצרת בקשת HTTP עם אימות בסיסי אל האתר שנמצא ב-URL שהוזן. באמצעות פעולה זו, ניתן לקבל גישה למידע המוגן באתר או לפנות ל- API עם אימות בסיסי.

## Deep Dive
כאשר משתמשים באימות בסיסי בשליחת בקשת HTTP, המידע של שם המשתמש והסיסמה שמוסרים בבקשה מוצפן בבסיס 64. זה מאפשר לנו לשלוח בקשה עם נתוני אימות מוצפנים באופן בטוח.

כדי לקבל את התוצאה של הבקשה, ניתן להשתמש בפונקציות כמו map או andThen כדי לעבד את המידע המתקבל מהבקשה. ניתן גם להשתמש בתנאי לבדוק את נכונות האימות ולטפל במקרים בהם האימות נכשל.

## ראה גם
כתב העת הרשמי של Elm: https://elm-lang.org/

API המכיל מידע על בקשות HTTP ב- Elm: https://package.elm-lang.org/packages/elm/http/latest/Http

מדריך לכתיבת אפליקציות מתקדמות ב- Elm: https://korban.net/posts/elm/2019-08-26-advanced-elm-function-composition/