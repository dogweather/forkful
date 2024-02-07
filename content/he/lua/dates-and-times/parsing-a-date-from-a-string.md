---
title:                "פרסום תאריך ממחרוזת"
date:                  2024-02-03T19:15:21.095187-07:00
model:                 gpt-4-0125-preview
simple_title:         "פרסום תאריך ממחרוזת"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/lua/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
לעבד תאריך ממחרוזת כולל המרה של ייצוגים טקסטואליים של תאריכים וזמנים לפורמט שניתן לעבד בקלות, לאחסן או להשוות בתוך תוכנית Lua. מתכנתים מבצעים משימה זו כדי להקל על פעולות כמו לוחות זמנים, רישום, או כל חישובי זמנים וכדי לגשר על הפער בין פורמטים קריאים לאדם של תאריכים ובין סוגי נתונים מובנים שמחשב יכול לעבד ביעילות.

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
