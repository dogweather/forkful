---
title:                "הורדת דף אינטרנט"
html_title:           "C++: הורדת דף אינטרנט"
simple_title:         "הורדת דף אינטרנט"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## מה זה ולמה?
הורדת דף אינטרנט היא הפעולה של משיכת מידע מתוך אתר אינטרנט ושמירתו על המחשב שלנו. מתכנתים משתמשים בכך בדרך כלל לצורך איסוף ועיבוד מידע אוטומטי מאתרי אינטרנט או API.

## שיטת מפעל
```Haskell
import Network.HTTP
import Network.URI (parseURI)

downloadURL :: String -> IO (Either String String)
downloadURL url = case parseURI url of
    Nothing -> return $ Left ("Invalid URL " ++ url)
    Just u  -> do
        eresp <- simpleHTTP (mkRequest GET u)
        return $ case eresp of
          Left err -> Left (show err)
          Right resp -> Right (rspBody resp)
```
הפעלת הפונקציה תחזיר את התכנים של האתר, או הודעת שגיאה במקרה שהאתר לא יכול להשתלשל.

## עומק שם
הרעיון להורדת דפים של אינטרנט התפתח לכנותה של האינטרנט כמקור עצום ומתעדכן של מידע. ישנם אלטרנטיבות אחרות ל-Haskell למשימה זו, כולל Python (עם BeautifulSoup) ו-JavaScript (עם Node.js). לעיתים מתכנתים משתמשים בסיפריות מיוחדות שמאפשרות להתממשק ישירות עם ה-API של אתר אינטרנט ולא להוריד את הדף שלם, ומאפשרות התממשקות הרבה יותר מהירה ויעילה.

## ראה גם
דוקומנטציה רשמית של Network.HTTP: https://hackage.haskell.org/package/HTTP
ספר עיון ל-Bash scripting: https://www.gnu.org/software/bash/manual/bash.html
Python Beautiful Soup Documentation: https://www.crummy.com/software/BeautifulSoup/bs4/doc/
Node.js Request Documentation: https://www.npmjs.com/package/request