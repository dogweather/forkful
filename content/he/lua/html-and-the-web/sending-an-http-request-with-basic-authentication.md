---
date: 2024-01-20 18:02:10.472033-07:00
description: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05E2\u05DD\
  \ \u05D0\u05D9\u05DE\u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9 \u05DE\u05D0\u05E4\
  \u05E9\u05E8\u05EA \u05D2\u05D9\u05E9\u05D4 \u05DE\u05D0\u05D5\u05D1\u05D8\u05D7\
  \u05EA \u05DC\u05DE\u05E9\u05D0\u05D1\u05D9\u05DD \u05D1\u05D0\u05D9\u05E0\u05D8\
  \u05E8\u05E0\u05D8. \u05EA\u05D5\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05DE\
  \u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\
  \u05D4\u05D1\u05D8\u05D9\u05D7 \u05E9\u05E8\u05E7 \u05DE\u05E9\u05EA\u05DE\u05E9\
  \u05D9\u05DD \u05DE\u05D5\u05E8\u05E9\u05D9\u05DD \u05D9\u05DB\u05D5\u05DC\u05D9\
  \u05DD \u05DC\u05E7\u05D1\u05DC \u05D0\u05D5 \u05DC\u05E9\u05DC\u05D5\u05D7 \u05DE\
  \u05D9\u05D3\u05E2."
lastmod: '2024-02-25T18:49:37.789777-07:00'
model: gpt-4-1106-preview
summary: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05E2\u05DD\
  \ \u05D0\u05D9\u05DE\u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9 \u05DE\u05D0\u05E4\
  \u05E9\u05E8\u05EA \u05D2\u05D9\u05E9\u05D4 \u05DE\u05D0\u05D5\u05D1\u05D8\u05D7\
  \u05EA \u05DC\u05DE\u05E9\u05D0\u05D1\u05D9\u05DD \u05D1\u05D0\u05D9\u05E0\u05D8\
  \u05E8\u05E0\u05D8. \u05EA\u05D5\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05DE\
  \u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\
  \u05D4\u05D1\u05D8\u05D9\u05D7 \u05E9\u05E8\u05E7 \u05DE\u05E9\u05EA\u05DE\u05E9\
  \u05D9\u05DD \u05DE\u05D5\u05E8\u05E9\u05D9\u05DD \u05D9\u05DB\u05D5\u05DC\u05D9\
  \u05DD \u05DC\u05E7\u05D1\u05DC \u05D0\u05D5 \u05DC\u05E9\u05DC\u05D5\u05D7 \u05DE\
  \u05D9\u05D3\u05E2."
title: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05E2\u05DD\
  \ \u05D0\u05D9\u05DE\u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9"
---

{{< edit_this_page >}}

## מה ולמה?
שליחת בקשת HTTP עם אימות בסיסי מאפשרת גישה מאובטחת למשאבים באינטרנט. תוכניתנים משתמשים בזה כדי להבטיח שרק משתמשים מורשים יכולים לקבל או לשלוח מידע.

## איך לעשות:
בלואה, נשתמש בספריית `socket.http` לשליחת הבקשות. תחילה, נרשום פונקציה להצפנת שם המשתמש והסיסמה:

```Lua
local http = require("socket.http")
local ltn12 = require("ltn12")
local mime = require("mime")

function createBasicAuth(username, password)
  local auth = mime.b64(username .. ":" .. password)
  return "Basic " .. auth
end

local response = {}
local username = "your_username"
local password = "your_password"
local authHeader = createBasicAuth(username, password)
local r, c, h = http.request {
  url = "http://your_api_endpoint_here",
  headers = {
    ["Authorization"] = authHeader
  },
  sink = ltn12.sink.table(response)
}

if c == 200 then
  print("Success:", table.concat(response))
else
  print("Error:", c)
end
```

זה ידפיס `"Success:"` ואת תוכן התשובה או `"Error:"` ואת הקוד המצבית אם שליחת הבקשה לא הצליחה.

## עיון מעמיק:
שליחת בקשות עם אימות בסיסי היא טכניקה עתיקה. היא שימושית אך לא בטוחה כמו גישות אימות מודרניות יותר כמו OAuth. בסיסי אומר שהמידע מועבר בצורה מקודדת ב-Base64, אך לא מוצפן.

בלואה, אנחנו לא מקבלים ספריית HTTP מובנית כמו בשפות אחרות, אז אנחנו משתמשים ב-`socket.http`. יש ספריות חיצוניות כמו `LuaSec` המוסיפות תמיכה בחיבורים מאובטחים (HTTPS), שהוא חשוב הרבה יותר לשימוש אינטרנטי מאובטח.

## ראה גם:
- [LuaSec](https://github.com/brunoos/luasec) - ספרייה לחיבורים מאובטחים בלואה.
- [LuaSocket HTTP Documentation](http://w3.impa.br/~diego/software/luasocket/http.html) - מסמכים ל-soket.http.
- [RFC 7617 'The 'Basic' HTTP Authentication Scheme'](https://tools.ietf.org/html/rfc7617) - מפרט טכני של אימות בסיסי.
