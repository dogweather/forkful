---
date: 2024-01-20 17:44:07.426520-07:00
description: "\u05D4\u05D5\u05E8\u05D3\u05EA \u05D3\u05E3 \u05D0\u05D9\u05E0\u05D8\
  \u05E8\u05E0\u05D8 \u05D6\u05D4 \u05E4\u05E9\u05D5\u05D8 \u05DC\u05E9\u05DC\u05D5\
  \u05E3 \u05EA\u05DB\u05E0\u05D9\u05DD \u05DE\u05D4\u05E8\u05E9\u05EA. \u05EA\u05DB\
  \u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4\
  \ \u05DB\u05D3\u05D9 \u05DC\u05E0\u05EA\u05D7 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD\
  , \u05DC\u05D1\u05D3\u05D5\u05E7 \u05E9\u05E8\u05EA\u05D9\u05DD, \u05D5\u05DC\u05D0\
  \u05D5\u05D8\u05D5\u05DE\u05D8 \u05EA\u05D4\u05DC\u05D9\u05DB\u05D9\u05DD."
lastmod: '2024-03-13T22:44:39.411362-06:00'
model: gpt-4-1106-preview
summary: "\u05D4\u05D5\u05E8\u05D3\u05EA \u05D3\u05E3 \u05D0\u05D9\u05E0\u05D8\u05E8\
  \u05E0\u05D8 \u05D6\u05D4 \u05E4\u05E9\u05D5\u05D8 \u05DC\u05E9\u05DC\u05D5\u05E3\
  \ \u05EA\u05DB\u05E0\u05D9\u05DD \u05DE\u05D4\u05E8\u05E9\u05EA. \u05EA\u05DB\u05E0\
  \u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\
  \u05D3\u05D9 \u05DC\u05E0\u05EA\u05D7 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05DC\
  \u05D1\u05D3\u05D5\u05E7 \u05E9\u05E8\u05EA\u05D9\u05DD, \u05D5\u05DC\u05D0\u05D5\
  \u05D8\u05D5\u05DE\u05D8 \u05EA\u05D4\u05DC\u05D9\u05DB\u05D9\u05DD."
title: "\u05D4\u05D5\u05E8\u05D3\u05EA \u05D3\u05E3 \u05D0\u05D9\u05E0\u05D8\u05E8\
  \u05E0\u05D8"
weight: 42
---

## מה ולמה?
הורדת דף אינטרנט זה פשוט לשלוף תכנים מהרשת. תכנתים עושים את זה כדי לנתח נתונים, לבדוק שרתים, ולאוטומט תהליכים.

## איך לעשות:
Haskell מאוד מתאים לסקריפטים קצרים ומתוחכמים. נשתמש בחבילת `http-conduit` כדי להוריד דף אינטרנט:

```Haskell
import Network.HTTP.Simple

-- קוד להורדת דף אינטרנט
downloadPage :: String -> IO ()
downloadPage url = do
    response <- httpBS url
    putStrLn $ "Status Code: " ++ show (getResponseStatusCode response)
    putStrLn $ "Body: " ++ show (getResponseBody response)

main :: IO ()
main = downloadPage "http://example.com"

-- תוצאה:
-- Status Code: 200
-- Body: "...(תוכן הדף)..."
```

הרצה פשוטה בקונסול תשיג את תוצאות הדף.

## עיון נוסף
הורדת דפי אינטרנט היא טכניקה עתיקה כמו האינטרנט עצמו. בשנים הראשונות, שימשו כלים כמו `wget` ו`curl`. ב-Haskell, חבילות כמו `http-conduit` ו`wreq` הן נפוצות, אך דורשות הבנה של מונדות ו-IO. יתרון של `http-conduit` הוא הטיפול המובנה ב-connections pool והניהול של זרימת הנתונים.

## ראו גם
- [`http-conduit` on Hackage](https://hackage.haskell.org/package/http-conduit)
- [The `wget` Manual](https://www.gnu.org/software/wget/manual/wget.html)
- [The `curl` Project](https://curl.se/docs/manual.html)
