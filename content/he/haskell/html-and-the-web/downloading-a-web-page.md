---
title:                "הורדת דף אינטרנט"
aliases: - /he/haskell/downloading-a-web-page.md
date:                  2024-01-20T17:44:07.426520-07:00
model:                 gpt-4-1106-preview
simple_title:         "הורדת דף אינטרנט"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

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
