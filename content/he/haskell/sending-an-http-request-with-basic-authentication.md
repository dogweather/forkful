---
title:                "שליחת בקשת http עם אימות בסיסי"
html_title:           "C: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## מה ולמה?

שליחת בקשת HTTP עם אימות בסיסי הוא מנגנון שמאפשר לנו לשלוח בקשות אל שרת אינטרנט, תוך שניםן מספקות פרטים לזיהוי המשתמש. מתכנתים משתמשים בכך כדי לגבול גישה למשאבים חשיפים יותר, או כאשר מדובר במשאבים שדורשים הרשאה מיוחדת.

## איך לעשות זאת:
בהנחה שיש לך מותקנת הספרייה `http-conduit`, הנה דרך אחת לשליחת בקשת HTTP עם אימות בסיסי:
```Haskell
import Network.HTTP.Simple
import Network.HTTP.Client ()  -- for the orphan instance

main :: IO ()
main = do
    request' <- parseRequest "http://httpbin.org/get"
    let request = setRequestBasicAuth "username" "password" request'
    response <- httpLBS request

    putStrLn $ "Status code: " ++ show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    putStrLn $ B.unpack $ getResponseBody response
```
ספריית ה `http-conduit` תאפשר לך לעבוד עם בקשות HTTP בצורה פשוטה יותר.

## הצצה עמוקה
שליחת בקשת HTTP עם אימות בסיסי היא מנגנון שנמצא בשימוש מאז התחלת שנות ה-90, והוא מתבצע על ידי הוספת כותרת `Authorization` לבקשה. למרות שישנן כמה אלטרנטיבות (כמו אימות מרובה גורמים), השימוש באימות בסיסי הוא עדיין שיטה נפוצה במיוחד במקרים שבהם שימוש בשיטות אימות מתקדמות יותר בלתי אפשרית.

## ראו גם
תוכל למצוא מידע נוסף בעמודים הבאים:
- [Haskell ספריית http-conduit](https://hackage.haskell.org/package/http-conduit)
- [אימות בסיסי בWikipedia](https://he.wikipedia.org/wiki/%D7%90%D7%99%D7%9E%D7%95%D7%AA_%D7%91%D7%91%D7%A7%D7%A9%D7%95%D7%AA_HTTP)