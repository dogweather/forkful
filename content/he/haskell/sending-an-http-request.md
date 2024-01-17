---
title:                "שליחת בקשת http"
html_title:           "Haskell: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## מה זה ולמה?
שליחת בקשת HTTP היא פעולה שמאפשרת לתכנתנים ליצור תקשורת בין יישומים. היא מאפשרת לשלוח בקשות מכלל אינטרנט כמו אינטרנט או ​HTTP

כלומר, זהו כלי מועיל לתכנתנים שמאפשר להשאיר את קוד התיקשורת למכונה. זה מאפשר לתכנתנים לפתר את בעיות התקשורת ולתפשר עם קוד אחר.

## איך לעשות זאת?
הנה דוגמא של שליחת דרך בקשת HTTP בהשתמשות בשפת Haskell.
```Haskell
import Network.HTTP

main :: IO ()
main = do
    response <- simpleHTTP (getRequest "http://google.com")
    html <- getResponseBody response
    putStrLn html
```
פלט:
```Haskell 
<html>...</html>
```

## נכנסים לעומק
זה שליחת בקשת HTTP היא כלי שקיים כבר למעלה מז' 30 שנים ומשמש לפתרון בעיות בתגובה של תיקשורת של העץ של המערכת. למעשה, קוד התקשורת הוא הרבה יותר פשוט וקל לתחזר, בעזרת זכרונות כלי הכמלול המכיל בקשת תקשורת.

יש גם חפשת בעל דרך חשוב לתקשורת עם המערכת שאינו תגובה ולא בהתאם לבעיה. המערכת זו כלל כלים בני שליחת תקשורת ובסופו של כל יום לעבוד בצורה האחרונה כל יום. לכן, ככל יום הם יכוחים וככל יום הם יכוחים על עליו.

## ראה גם
- [Haskell בוויקיפדיה](https://he.wikipedia.org/wiki/Haskell)
- [Network.HTTP בהובהאב](https://hackage.haskell.org/package/HTTP)
- [שליחת בקשת HTTP בשפת התיכונה](https://www.geeksforgeeks.org/http-request-in-middle-of-network-programming-c/)