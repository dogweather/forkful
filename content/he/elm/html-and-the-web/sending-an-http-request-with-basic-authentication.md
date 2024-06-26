---
date: 2024-01-20 18:01:28.755378-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05DB\u05D0\u05DF\
  \ \u05D0\u05EA\u05DD \u05E8\u05D5\u05D0\u05D9\u05DD \u05D4\u05D2\u05D3\u05E8\u05EA\
  \ \u05D4\u05DB\u05D5\u05EA\u05E8\u05D5\u05EA \u05D5\u05E9\u05DC\u05D9\u05D7\u05EA\
  \ \u05D1\u05E7\u05E9\u05D4 \u05E2\u05DD \u05D0\u05D9\u05DE\u05D5\u05EA \u05D1\u05E1\
  \u05D9\u05E1\u05D9. `GotData` \u05D4\u05D5\u05D0 \u05D4\u05D5\u05D3\u05E2\u05D4\
  \ \u05E9\u05DE\u05D8\u05E4\u05DC\u05EA \u05D1\u05EA\u05D5\u05E6\u05D0\u05D4."
lastmod: '2024-04-05T21:53:40.419354-06:00'
model: gpt-4-1106-preview
summary: "\u05DB\u05D0\u05DF \u05D0\u05EA\u05DD \u05E8\u05D5\u05D0\u05D9\u05DD \u05D4\
  \u05D2\u05D3\u05E8\u05EA \u05D4\u05DB\u05D5\u05EA\u05E8\u05D5\u05EA \u05D5\u05E9\
  \u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05D4 \u05E2\u05DD \u05D0\u05D9\u05DE\
  \u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9."
title: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05E2\u05DD\
  \ \u05D0\u05D9\u05DE\u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9"
weight: 45
---

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
