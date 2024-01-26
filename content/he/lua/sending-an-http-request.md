---
title:                "שליחת בקשת HTTP"
date:                  2024-01-20T18:00:16.548621-07:00
model:                 gpt-4-1106-preview
simple_title:         "שליחת בקשת HTTP"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/lua/sending-an-http-request.md"
---

{{< edit_this_page >}}

## מה ולמה?
שליחת בקשת HTTP היא דרך לתקשר עם שרתי ווב לקבלת או שליחת מידע. תכניתנים עושים זאת כדי לאסוף נתונים, לבצע אימות, או לשלוח פקודות לשרתים מתוך יישומים שלהם.

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
