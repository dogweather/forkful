---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:21.095187-07:00
description: "\u05D0\u05D9\u05DA \u05DC: Lua \u05D0\u05D9\u05E0\u05D4 \u05DB\u05D5\
  \u05DC\u05DC\u05EA \u05EA\u05DE\u05D9\u05DB\u05D4 \u05DE\u05D5\u05D1\u05E0\u05D9\
  \u05EA \u05DC\u05E0\u05D9\u05E4\u05D5\u05D9 \u05D5\u05DC\u05D4\u05EA\u05DE\u05E6\
  \u05D0\u05D5\u05EA \u05E2\u05DD \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD \u05D5\
  \u05D6\u05DE\u05E0\u05D9\u05DD \u05DE\u05E2\u05D1\u05E8 \u05DC\u05E4\u05D5\u05E0\
  \u05E7\u05E6\u05D9\u05D5\u05E0\u05DC\u05D9\u05D5\u05EA \u05D4\u05DE\u05D5\u05D2\u05D1\
  \u05DC\u05EA \u05E9`os.date` \u05D5-`os.time` \u05DE\u05E1\u05E4\u05E7\u05D5\u05EA\
  . \u05E2\u05DD \u05D6\u05D0\u05EA, \u05E0\u05D9\u05EA\u05DF \u05DC\u05E0\u05E6\u05DC\
  \ \u05D2\u05E8\u05E1\u05D0\u05D5\u05EA\u2026"
lastmod: '2024-03-13T22:44:39.566575-06:00'
model: gpt-4-0125-preview
summary: "Lua \u05D0\u05D9\u05E0\u05D4 \u05DB\u05D5\u05DC\u05DC\u05EA \u05EA\u05DE\
  \u05D9\u05DB\u05D4 \u05DE\u05D5\u05D1\u05E0\u05D9\u05EA \u05DC\u05E0\u05D9\u05E4\
  \u05D5\u05D9 \u05D5\u05DC\u05D4\u05EA\u05DE\u05E6\u05D0\u05D5\u05EA \u05E2\u05DD\
  \ \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD \u05D5\u05D6\u05DE\u05E0\u05D9\u05DD\
  \ \u05DE\u05E2\u05D1\u05E8 \u05DC\u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05E0\
  \u05DC\u05D9\u05D5\u05EA \u05D4\u05DE\u05D5\u05D2\u05D1\u05DC\u05EA \u05E9`os.date`\
  \ \u05D5-`os.time` \u05DE\u05E1\u05E4\u05E7\u05D5\u05EA."
title: "\u05E4\u05E8\u05E1\u05D5\u05DD \u05EA\u05D0\u05E8\u05D9\u05DA \u05DE\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA"
weight: 30
---

## איך ל:
Lua אינה כוללת תמיכה מובנית לניפוי ולהתמצאות עם תאריכים וזמנים מעבר לפונקציונליות המוגבלת ש`os.date` ו-`os.time` מספקות. עם זאת, ניתן לנצל גרסאות אלו לניתוח בסיסי, ולדרישות מורכבות יותר, ניתן להשתמש בספריית `luadate`, ספרייה חיצונית.

**שימוש ב-`os.date` ו-`os.time`:**
```lua
-- המרת תאריך קריא לאדם לחותמת זמן ובחזרה
local dateString = "2023-09-21 15:00:00"
local pattern = "(%d+)-(%d+)-(%d+) (%d+):(%d+):(%d+)"
local year, month, day, hour, minute, second = dateString:match(pattern)

local timestamp = os.time({
  year = year,
  month = month,
  day = day,
  hour = hour,
  min = minute,
  sec = second
})

-- המרת חותמת זמן בחזרה לפורמט קריא לאדם
local formattedDate = os.date("%Y-%m-%d %H:%M:%S", timestamp)
print(formattedDate)  -- פלט: 2023-09-21 15:00:00
```

**שימוש ב-`luadate` (ספריית צד שלישי):**
כדי להשתמש ב-`luadate`, ודא שהיא מותקנת דרך LuaRocks או מנהל החבילות שבחרת. `luadate` מוסיפה יכולות ניתוח וניהול תאריכים וזמנים רחבות. 

```lua
local date = require('date')

-- ניתוח סטרינג של תאריך ישירות
local parsedDate = date.parse("2023-09-21 15:00:00")
print(parsedDate:fmt("%Y-%m-%d %H:%M:%S"))  -- פלט: 2023-09-21 15:00:00

-- הוספת משכים
local oneWeekLater = parsedDate:adddays(7)
print(oneWeekLater:fmt("%Y-%m-%d %H:%M:%S"))  -- פלט: 2023-09-28 15:00:00
```

הספרייה `luadate` מציעה דרך יותר אינטואיטיבית ועוצמתית לעבוד עם תאריכים, כולל ניתוח ממחרוזות, פורמטינג, ופעולות חשבוניות על תאריכים, דבר המפשט באופן משמעותי את העבודה עם נתונים זמניים ב-Lua.
