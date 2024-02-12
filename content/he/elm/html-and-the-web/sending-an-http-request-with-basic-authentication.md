---
title:                "שליחת בקשת HTTP עם אימות בסיסי"
date:                  2024-01-20T18:01:28.755378-07:00
model:                 gpt-4-1106-preview
simple_title:         "שליחת בקשת HTTP עם אימות בסיסי"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## מה ולמה?
שליחת בקשת HTTP עם אימות בסיסי היא תהליך שבו נשלחת בקשת רשת עם שם משתמש וסיסמה לאימות. תכנותים עושים זאת כדי לגשת למשאבים מוגנים באינטרנט.

## איך לעשות:
```Elm
import Http
import Base64

type Msg
    = GotData (Result Http.Error String)

basicAuth : String -> String -> List Http.Header
basicAuth username password =
    let
        credentials = username ++ ":" ++ password
        encodedCredentials = Base64.encode credentials
    in
    [ Http.header "Authorization" ("Basic " ++ encodedCredentials) ]

sendRequest : Cmd Msg
sendRequest =
    Http.get
        { url = "https://example.com/protected-resource"
        , headers = basicAuth "myUsername" "myPassword"
        , expect = Http.expectString GotData
        }
```

כאן אתם רואים הגדרת הכותרות ושליחת בקשה עם אימות בסיסי. `GotData` הוא הודעה שמטפלת בתוצאה.

## טבילה עמוקה
בעבר, אימות בסיסי בHTTP היה פופולרי לאימות פשוט כיוון שהוא קל ליישום. כיום, קיימות שיטות אימות חזקות יותר כמו OAuth 2.0, אבל אימות בסיסי עדיין בשימוש לצרכים מסוימים. בעיה עם אימות בסיסי היא ששם המשתמש והסיסמה מועברים בקוד מקור קל לחיקוי (base64) ולכן צריך לשלוח אותם תמיד מעל חיבור מאובטח (HTTPS).

כאשר מדובר בElm, הטיפול בHTTP מתבצע באמצעות חבילת `Http` הסטנדרטית. בפונקציה `basicAuth`, אנו בונים כותרות המכילות את האימות בBase64, ובפונקציה `sendRequest` אנו שולחים את הבקשה עצמה, מצפים לתשובה כטקסט ומשתמשים בהודעה כדי לטפל בסטטוס ובתשובה שנקבל.

## ראו גם
- מדריך עבור חבילת הHTTP של Elm: https://package.elm-lang.org/packages/elm/http/latest/
- איך להשתמש בבסיס 64 בElm: https://package.elm-lang.org/packages/truqu/elm-base64/latest/
- מידע על אימות בסיסי: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- מידע על פרוטוקול האימות OAuth 2.0: https://oauth.net/2/
