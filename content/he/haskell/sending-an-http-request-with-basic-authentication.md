---
title:                "Haskell: שליחת בקשת HTTP עם אימות בסיסי"
simple_title:         "שליחת בקשת HTTP עם אימות בסיסי"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

למה: רק 1-2 משפטים שמסבירים *למה* מישהו עשוי לבחור לשלוח בקשת HTTP עם אימות בסיסי.

### למה

HTTP עובד על פרוטוקול מכירות וכדי לקבל גישה למאגרי מידע, יש צורך להתחבר לשרת. כאן נכנסת לתמונה האימות הבסיסי, המאפשר למשתמש ולספק את הכניסה לשרת מהירה ופשוטה. כדי לכיפل בקשת HTTP עם אימות בסיסי, ניצור כותרת מסויימת בשורות הכותרת ונשתמש בפרמטרים מתאימים כדי ליצור את הכותרת הנכונה.

### איך לעשות

כאן ניתן לראות דוגמאות של קוד עבור בקשת HTTP עם אימות בסיסי בשפת הפנקס הקושרת המתאימה, Haskell.

```Haskell
import Network.HTTP.Client
import Network.HTTP.Types.Status
import Data.ByteString.UTF8 as BU
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

-- ליצור כותרת עם משתמש וסיסמה
-- יש לשנות את הערך למשתמש והסיסמה המתאימים
authHeader :: BU.ByteString
authHeader = "Basic " `B.append` (toBase64 "user:password")

-- לבצע בקשת GET עם אימות בסיסי לכתובת ספציפית
main :: IO()
main = do
    -- ליצור חיבור חדש לשרת
    manager <- newManager defaultManagerSettings
    -- להגדיר את הכתובת לבקשה
    request <- parseRequest "http://example.com"
    -- להוסיף את הכותרת הנחוצה לבקשה
    let request' = request { requestHeaders = [(hAuthorization, authHeader)] }
    -- לשלוח את הבקשה ולהתקבל את התשובה
    response <- httpLbs request' manager
    -- להדפיס את קוד התשובה כדי לוודא שהבקשה בוצעה בהצלחה
    putStrLn $ "Response code: " ++ (show $ statusCode $ responseStatus response)
    -- להציג את גוף התשובה
    BL.putStr $ responseBody response

-- פונקציה עזר להמרת מחרוזת לבייס חשוב
toBase64 :: [Char] -> B.ByteString
toBase64 = BU.fromString . base64 where
    base64 = map (toEnum . (+ 48) . (`mod` 64)) . toIndexes

-- פונקציה עזר למיפוי התוו