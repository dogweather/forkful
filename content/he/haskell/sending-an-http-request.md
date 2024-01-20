---
title:                "שליחת בקשת http"
html_title:           "Bash: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## מה & למה?
שליחת בקשת HTTP היא פעולה שבה המחשב שלך מבקש מידע או שינוי משרת אינטרנט. מתכנתים עושים זאת כדי לאפשר לאפליקציה שלהם להתכתב עם שאר הרשת.

## כיצד ל:
באמצעות מעטפת HTTP Simple, אתה יכול לשלוח בקשות HTTP בפשטות:

```Haskell
ghci> import Network.HTTP.Simple -- ייבא את המעטפת

ghci> let request = "http://httpbin.org/get?key=value" -- הגדרת הבקשה

ghci> response <- httpJSON $ parseRequest_ request -- שולחת את הבקשה ומבצעת פענוח JSON

ghci> print $ getResponseBody response -- הדפסת התגובה
```

פלט דוגמה:

```Haskell
("/{\"url\":\"http://httpbin.org/get?key=value\",\"headers\": ... ,\"args\":{\"key\":\"value\"}}")
```

## צלילה עמוקה:
אחרי שחרור Haskell בשנת 1990, אנשים התחילו להשתמש בקוד HTTP המותאם אישית, אך בגלל הקשיים עם אבטחה וניהילת חיבורים, ציוד בליבות אינטראקטיביות הפך להולך ונפוץ יותר. היום, ישנם ארבעה מעטפות HTTP מרכזיים ב- Haskell: http-client, http-simple, http-conduit, ו req. כל אחד מהם מספק התממשות שונה ואפשרויות שונות, אך המדגם למעלה משתמש מטפת HTTP Simple עקב פשטות שלה.

## ראה גם:
וויקיפדיה: [HTTP Requests](https://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol#Request_methods)
תיעוד Haskell: [SimpleHTTP](https://hackage.haskell.org/package/http-conduit/docs/Network-HTTP-Simple.html)