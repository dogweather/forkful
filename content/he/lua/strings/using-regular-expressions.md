---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:19.168564-07:00
description: "\u05D0\u05D9\u05DA \u05DC: \u05DC\u05D5\u05D0\u05D4 \u05D0\u05D9\u05E0\
  \u05D4 \u05EA\u05D5\u05DE\u05DB\u05EA \u05D1\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\
  \u05DD \u05E8\u05D2\u05D5\u05DC\u05E8\u05D9\u05D9\u05DD \u05D1\u05D0\u05D5\u05E4\
  \u05DF \u05D8\u05D1\u05E2\u05D9 \u05D1\u05D3\u05E8\u05DA \u05DB\u05DE\u05D5 \u05E9\
  \u05E4\u05D5\u05EA \u05DB\u05DE\u05D5 Perl \u05D0\u05D5 Python. \u05D1\u05DE\u05E7\
  \u05D5\u05DD \u05D6\u05D0\u05EA, \u05D4\u05D9\u05D0 \u05DE\u05E6\u05D9\u05E2\u05D4\
  \ \u05D9\u05DB\u05D5\u05DC\u05D5\u05EA \u05D4\u05EA\u05D0\u05DE\u05EA \u05D3\u05E4\
  \u05D5\u05E1\u05D9\u05DD \u05E9\u05DB\u05D5\u05DC\u05DC\u05D5\u05EA \u05DE\u05E7\
  \u05E8\u05D9 \u05E9\u05D9\u05DE\u05D5\u05E9 \u05E8\u05D1\u05D9\u05DD\u2026"
lastmod: '2024-03-13T22:44:39.534492-06:00'
model: gpt-4-0125-preview
summary: "\u05DC\u05D5\u05D0\u05D4 \u05D0\u05D9\u05E0\u05D4 \u05EA\u05D5\u05DE\u05DB\
  \u05EA \u05D1\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD \u05E8\u05D2\u05D5\u05DC\
  \u05E8\u05D9\u05D9\u05DD \u05D1\u05D0\u05D5\u05E4\u05DF \u05D8\u05D1\u05E2\u05D9\
  \ \u05D1\u05D3\u05E8\u05DA \u05DB\u05DE\u05D5 \u05E9\u05E4\u05D5\u05EA \u05DB\u05DE\
  \u05D5 Perl \u05D0\u05D5 Python."
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD\
  \ \u05E8\u05D2\u05D5\u05DC\u05E8\u05D9\u05D9\u05DD"
weight: 11
---

## איך ל:
לואה אינה תומכת בביטויים רגולריים באופן טבעי בדרך כמו שפות כמו Perl או Python. במקום זאת, היא מציעה יכולות התאמת דפוסים שכוללות מקרי שימוש רבים נפוצים של ביטויים רגולריים. עם זאת, לתמיכה מלאה בביטויים רגולריים, אפשר להשתמש בספריית צד שלישי כמו `lrexlib`.

### התאמת דפוסים בסיסית בלואה:
לואה מספקת מערכת התאמת דפוסים חזקה שאת/ה יכול/ה להשתמש בה לחיפושים והחלפות פשוטות:

```lua
-- חיפוש פשוט
local str = "שלום, עולם!"
if string.find(str, "עולם") then
  print("נמצאה התאמה!")
end
-- פלט: נמצאה התאמה!

-- החלפה פשוטה
local s = string.gsub("לואה מעולה!", "מעולה", "מדהים")
print(s)
-- פלט: לואה מדהים!
```

### לכידת תתי-מחרוזות:
את/ה יכול/ה ללכוד חלקים מהמחרוזת שתואמים לדפוסים:

```lua
local date = "היום הוא 17/05/2023."
local d, m, y = string.match(date, "(%d+)/(%d+)/(%d+)")
print("יום:", d, "חודש:", m, "שנה:", y)
-- פלט: יום: 17 חודש: 05 שנה: 2023
```

### שימוש ב-`lrexlib` לביטויים רגולריים:
לשימוש בביטויים רגולריים אמיתיים, את/ה יכול/ה להתקין ולהשתמש ב-`lrexlib`. בהנחה שהותקן (`luarocks install lrexlib-pcre`), את/ה יכול/ה לעשות התאמות דפוסים מורכבות יותר:

```lua
local rex = require 'rex_pcre'

local text = "הגשם בספרד נשאר בעיקר במישור."
local regex = "\\bS\\w+"
local count, err = rex.gsub(text, regex, function(w)
  return w:upper()
end)
if err then
  print("שגיאה:", err)
else
  print("טקסט שונה:", text)
  print("החלפות שבוצעו:", count)
end
-- דוגמא לפלט: טקסט שונה: הגשם בספרד נשאר בעיקר במישור.
-- החלפות שבוצעו: 3
```

הדוגמאות לעיל ממחישות שימוש בסיסי במערכת התאמת הדפוסים של לואה ואיך להשתמש בכוח של ביטויים רגולריים באמצעות `lrexlib`. בין אם את/ה מבצע/ת שינויים פשוטים במחרוזות או דורש/ת את כל הגמישות המוצעת על ידי ביטויים רגולריים, לואה, בשילוב עם ספריות חזקות, יכולה לענות על צרכיך.
