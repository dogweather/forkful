---
title:                "שליחת בקשת http"
html_title:           "Lua: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/lua/sending-an-http-request.md"
---

{{< edit_this_page >}}

## מה ולמה?
שליחת בקשת HTTP היא פעולה שמאפשרת למתכנתים לשלוח בקשות לשרתים רחוקים ולקבל תגובות מכינם. מתכנתים משתמשים בכך לשלוח ולקבל נתונים, לדוגמה בעת גישה לאתרים אינטרנט או ליצירת אפליקציות מבוססות אינטרנט.

## איך לעשות:
תרגולים קוד ותוצאות דוגמה יופיעו בתוך בלוקי קוד ```Lua ... ```.

```Lua
-- משלוח בקשת GET לאתר אינטרנט והדפסת תוכן התגובה
local https = require("ssl.https")
local body, code, headers, status = https.request("https://www.example.com")
print(code)
print(body)
```

כמו כן, ניתן גם לשלוח בקשות באמצעות הספרייה socket שכוללת מתאם של HTTP.

```Lua
-- משלוח בקשת POST עם נתונים והדפסת תוצאת הבקשה
local socket = require("socket")
local host = "www.example.com"
local port = 80
local path = "/login"
local data = "username=test&password=test123"

local conn = socket.tcp()
conn:connect(host, port)
conn:send("POST " .. path  .. " HTTP/1.1\r\n")
conn:send("Content-Length: " .. string.len(data) .. "\r\n\r\n")
conn:send(data)
local response = conn:receive("*a")
print(response)
```

## מעמקים:
שיטת ה-sending HTTP request היא חלק חשוב בעבודת פיתוח ומשתמשים בה כבר מאז תחילת ימי האינטרנט. ישנן גם אפשרויות נוספות כגון שימוש בפרוטוקולים אחרים כגון FTP או SMTP במקום HTTP. גם טכנולוגיות חדשות יותר כגון GraphQL מאפשרות חיפוש מתקדם ומציאת נתונים נוספים על פי דרישות מסוימות.

עוד אפשרויות לשליחת בקשות HTTP הן תוך שימוש בפתיחה ידנית של חיבור עם השרת ושימוש בפרוטוקולים נוספים כמו HTTPS להבטחת תקשורת מאובטחת.

## ראו גם:
למידע נוסף ודוגמאות נוספות, התייחסות לפונקציות וסינטקס של שליחת HTTP requests בשימוש בשפת לואה, ניתן לעיין במדריכים באתרים המפורטים מטה:

- [Lua.org](https://www.lua.org)
- [OpenResty](https://openresty.org)
- [lua-requests](https://github.com/aclark4life/lua-requests)