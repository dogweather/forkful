---
date: 2024-01-20 18:00:16.548621-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1\u05E7\u05D8\
  \u05E2 \u05D4\u05D6\u05D4 \u05E0\u05E8\u05D0\u05D4 \u05D3\u05D5\u05D2\u05DE\u05D4\
  \ \u05E4\u05E9\u05D5\u05D8\u05D4 \u05DC\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\
  \u05E9\u05EA HTTP GET \u05D1\u05E9\u05E4\u05EA \u05DC\u05D5\u05D0\u05D4."
lastmod: '2024-03-13T22:44:39.545699-06:00'
model: gpt-4-1106-preview
summary: "\u05D1\u05E7\u05D8\u05E2 \u05D4\u05D6\u05D4 \u05E0\u05E8\u05D0\u05D4 \u05D3\
  \u05D5\u05D2\u05DE\u05D4 \u05E4\u05E9\u05D5\u05D8\u05D4 \u05DC\u05E9\u05DC\u05D9\
  \u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP GET \u05D1\u05E9\u05E4\u05EA \u05DC\u05D5\
  \u05D0\u05D4."
title: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP"
weight: 44
---

## איך לעשות:
בקטע הזה נראה דוגמה פשוטה לשליחת בקשת HTTP GET בשפת לואה.

```Lua
local http = require("socket.http")  -- טעינת המודול ברשת של לואה
local body, code, headers, status = http.request("http://example.com")  -- בקשת GET לדוגמא

-- הדפסת תוצאות הבקשה
print("HTTP קוד סטטוס: " .. code)  -- ידפיס, נניח, "HTTP קוד סטטוס: 200"
if code == 200 then
    print("תוכן התגובה: " .. body) -- ידפיס את הHTML של הדף
end
```

כאשר מריצים את הקוד הזה, התוצאה תהיה תשובה מהשרת של האתר שהוא ביקש, שהוא `example.com` במקרה הזה.

## עומק הצלילה
שליחת בקשות HTTP משתמשת בפרוטוקול שקיים מאז שנות ה-90 ומשמש לתקשורת ברשת האינטרנט. אף על פי שיש ספריות רבות ומגוונות לשליחת בקשות HTTP בשפות תכנות שונות, בלואה אנו לעיתים קרובות משתמשים במודול `socket.http`, החלק מהספריה `LuaSocket`. קיימות גם אלטרנטיבות כמו `curl` ו-`luasec` לבקשות מאובטחות יותר (HTTPS).

כאשר שולחים בקשה, הפרוטוקול דורש שנגדיר פרטים כגון סוג הבקשה (GET, POST, וכו'), שם המארח (host name), פאת' למשאב הנדרש, ולעיתים גם פרטים נוספים כגון פרמטרים וכותרות (headers).

## ראה גם
- LuaSocket (באנגלית) - [http://w3.impa.br/~diego/software/luasocket](http://w3.impa.br/~diego/software/luasocket)
- LuaSec (לבקשות HTTPS, באנגלית) - [https://github.com/brunoos/luasec/wiki](https://github.com/brunoos/luasec/wiki)
