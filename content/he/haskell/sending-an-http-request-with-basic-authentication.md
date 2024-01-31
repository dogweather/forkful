---
title:                "שליחת בקשת HTTP עם אימות בסיסי"
date:                  2024-01-20T18:01:58.956717-07:00
model:                 gpt-4-1106-preview
simple_title:         "שליחת בקשת HTTP עם אימות בסיסי"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## מה ולמה?
שליחת בקשת HTTP עם אימות בסיסי מאפשרת גישה מאובטחת למשאבים ברשת. תוכניתנים משתמשים בזה כדי להבטיח שרק משתמשים עם הרשאות נכונות יכולים לגשת למידע רגיש.

## איך לעשות:
כדי לשלוח בקשה עם אימות בסיסי ב-Haskell, ניתן להשתמש בחבילה `http-conduit` ביחד עם `Network.HTTP.Simple`.

```Haskell
import Network.HTTP.Simple
import Network.HTTP.Types.Header (hAuthorization)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Base64 (encode)

-- יצירת כותרת אימות בסיסית
basicAuth :: String -> String -> Header
basicAuth username password = (hAuthorization, "Basic " <> encode (pack (username <> ":" <> password)))

-- פונקציה לביצוע בקשת GET עם אימות בסיסי
makeRequest :: String -> String -> String -> IO ()
makeRequest url username password = do
    let request = setRequestMethod "GET"
                  $ setRequestHeaders [basicAuth username password]
                  $ defaultRequest
                  { getUri = Just (URI.parseURI url) }
    response <- httpLBS request
    putStrLn $ "Status code: " ++ show (getResponseStatusCode response)
    print $ getResponseBody response

-- שימוש בפונקציה
main :: IO ()
main = makeRequest "http://example.com/protected" "user" "pass"
```

תוצאת הדוגמה:
```
Status code: [קוד המצב שהתקבל מהשרת]
[תגובת הגוף שהתקבלה מהשרת]
```

## עיון עמוק:
שליחת בקשות עם אימות בסיסי היא טכניקה כבר קיימת מאז הימים הראשונים של האינטרנט. אימות בסיסי אינו הכי בטוח כיוון שהוא מצפין את שם המשתמש והסיסמה ב-base64, שאינה הצפנה חזקה. ישנם שיטות אימות מתקדמות יותר כמו OAuth, אבל לפעמים אימות בסיסי מספיק.

בעת ביצוע בקשת אימות בסיסי ב-Haskell, נחוץ להתמודד עם חבילות חיצוניות ולהבין כיצד מתבצעת קידוד ל-base64. כאשר יוצרים את כותרת האימות, יש לשים לב לכך ששם המשתמש והסיסמה מחוברים עם נקודותיים ולאחר מכן מקודדים ל-base64 ומוספים את המילה "Basic" לפני הטקסט המקודד.

## ראה גם:
- המדריך ל-HTTP במסמכי MDN: [MDN HTTP Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- תיעוד החבילה `http-conduit`: [http-conduit on Hackage](https://hackage.haskell.org/package/http-conduit)
- תיעוד חבילת `base64-bytestring`: [base64-bytestring on Hackage](https://hackage.haskell.org/package/base64-bytestring)
