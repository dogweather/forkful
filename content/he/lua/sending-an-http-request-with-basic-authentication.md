---
title:                "שליחת בקשת http עם אימות בסיסי"
html_title:           "C: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/lua/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## מה ולמה?
שליחת בקשת HTTP עם אימות בסיסי היא תהליך שבו נשלחת בקשה למשאב על גבי הרשת תוך שימוש בזיהוי בסיסי. מתכנתים משתמשים בזה כדי לאמת את המשתמש ששולח את הבקשה.

## איך לעשות:
הדוגמה הבאה משתמשת בספריית Lua `http` לשליחת בקשת `GET` מאומתת.
```Lua
local http = require("socket.http")
local ltn12 = require("ltn12")

-- נתונים לאימות
local user = "username"
local password = "password"
local auth = "Basic " .. (user .. ":" .. password):b64()

-- בקשה
local response = {}
http.request
{
  url = "http://example.com",
  sink = ltn12.sink.table(response),
  headers = {
    authorization = auth
  }
}

-- הדפסת תגובה
for i, line in ipairs(response) do
  print(line)
end
```
הקוד שלנו משתמש בפרטים של אימות ושולח בקשה `GET` לאתר הדוגמה. התגובה מדפיסה למסך.

## צלילה עמוקה:
שליחת בקשת HTTP עם אימות בסיסי הייתה אחת מהשיטות הראשונות לאמת משתמשים באינטרנט. יש שיטות אמת רבות אחרות היום, למשל OAuth וJWT. ראשי לחוק הישראלי, עליך להקפיד על אבטחת המידע והזיהוי של המשתמשים.

## ראה גם:
- [מרכז עזרה של LuaSocket](http://w3.impa.br/~diego/software/luasocket/http.html)
- [דוקומנטציה של HTTP Basic Authentication ב-MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [מדריך להגנה על מידע אישי](https://www.itpc.gov.il/MY-Privacy/34-4)