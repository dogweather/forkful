---
title:                "שליחת בקשת http עם אימות בסיסי"
html_title:           "Lua: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/lua/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

כשמדברים על בקשות HTTP עם אימות בסיסי, אנחנו מתכוונים לבקשה שנשלחת מעבר לרשת האינטרנט כדי לקבל גישה למידע שמוגן על ידי שם משתמש וסיסמה. מתכנתים משתמשים בזה כדי לאפשר גישה מאובטחת למרכזי נתונים או שירותים בענן.

```Lua
local http = require("socket.http")
local ltn12 = require("ltn12")

-- יישום פשוט יותר
-- בניית ספריה כדי לקבל גישה לאותו הספק.
local request_body = '{"username": "myusername", "password": "mypassword"}'
local response_body = {}
local res, code, response_headers =
        http.request{
                url = "https://example.com/api/getData",
                method = "POST",
                headers = {
                        ["Content-Type"] = "application/json",
                        ["Content-Length"] = #request_body
                        ["Authorization"] = "Basic " .. (mime.b64("myusername:mypassword")
                },
                source = ltn12.source.string(request_body),
                sink = ltn12.sink.table(response_body),
        }

-- חזרה על הקודת מצב והגוף של התגובה
print(code, res)

-- קבלת הספרה שהובאה בגוף התגובה
print(response_body[1])
```

HTTP נמצאת איתנה מאז שהוחלה תוקפה רציפה של ברודוול, בלשון המעטה. בתרגום די פשוט לתנאים טכניים, מדובר בזרם של טקסט בפרוטוקול TCPIP תוך שימת קבצי הדפדפן כפונטים עוקבים המבנה כאשר הנג