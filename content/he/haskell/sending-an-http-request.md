---
title:                "Haskell: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

# למה?

למה לצורך בעיתון בHaskell, אנשים עשויים להתעסק בשליחת בקשת HTTP כדי ליצור תקשורת בין אפליקציות שונות, לגשת למידע או לבצע פעולות במקומות רחוקים.

# איך לבצע?

```Haskell
import Network.HTTP

main :: IO ()
main = do
  response <- simpleHTTP (getRequest "https://www.example.com")
  responseBody <- getResponseBody response
  putStrLn responseBody
```

כאן אנו משתמשים בספריית Network.HTTP כדי לשלוח בקשת HTTP בקוד Haskell. נחשוב על זה כפעולת גשת לכתובת URL מסוימת, ולקבל תוצאה בחזרה, בצורת טקסט. בדוגמה זו, אנו מבצעים בקשה פשוטה לאתר דוגמה ומדפיסים את התוכן של התגובה.

# שוקלים עמוק יותר

שליחת בקשת HTTP בקוד Haskell היא רק ההתחלה. ישנם הרבה אפשרויות להתאים ולכוון את הבקשה כדי להתאים טוב יותר לצרכים שלך. ניתן לשנות את שלב הבקשה, לעדכן את התוכן, להוסיף כותרת או כל פרמטר אחר שיש לו צורך. ישנם גם הרבה ספריות נוספות זמינות המספקות תיכנות בקשות מתקדמות באופן יעיל ומנותק.

# ראו גם

- [Haskell-What is HTTP in Haskell?](https://www.educba.com/haskell-http/)
- [Sending HTTP Requests in Haskell with Wreq](https://www.fpcomplete.com/blog/2017/07/sending-http-requests-in-haskell-with-wreq/)
- [Network.HTTP – Hackage](https://hackage.haskell.org/package/HTTP)