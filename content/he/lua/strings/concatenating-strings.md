---
date: 2024-01-20 17:35:14.696616-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D4\u05E6\u05D9\
  \u05E8\u05D5\u05E3 \u05D4\u05D9\u05D4 \u05D7\u05DC\u05E7 \u05DE-Lua \u05DE\u05D4\
  \u05E8\u05D2\u05E2 \u05D4\u05E8\u05D0\u05E9\u05D5\u05DF. \u05D1\u05E9\u05E0\u05D9\
  \u05DD \u05D4\u05E8\u05D0\u05E9\u05D5\u05E0\u05D5\u05EA, \u05E6\u05D9\u05E8\u05D5\
  \u05E3 \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05D4\u05D9\u05D4 \u05E4\u05D7\
  \u05D5\u05EA \u05D9\u05E2\u05D9\u05DC, \u05D0\u05DA \u05DC\u05D0\u05D5\u05E8\u05DA\
  \ \u05D4\u05E9\u05E0\u05D9\u05DD \u05D4\u05D5\u05E4\u05DA \u05DC\u05D4\u05D9\u05D5\
  \u05EA \u05D1\u05D9\u05E6\u05D5\u05E2\u05D9\u05EA. \u05D7\u05DC\u05D5\u05E4\u05D5\
  \u05EA \u05DC\u05E6\u05D9\u05E8\u05D5\u05E3 \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\
  \u05EA \u05DB\u05D5\u05DC\u05DC\u05D5\u05EA\u2026"
lastmod: '2024-04-05T21:53:40.677187-06:00'
model: gpt-4-1106-preview
summary: "\u05D4\u05E6\u05D9\u05E8\u05D5\u05E3 \u05D4\u05D9\u05D4 \u05D7\u05DC\u05E7\
  \ \u05DE-Lua \u05DE\u05D4\u05E8\u05D2\u05E2 \u05D4\u05E8\u05D0\u05E9\u05D5\u05DF\
  ."
title: "\u05E9\u05E8\u05E9\u05D5\u05E8 \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA"
weight: 3
---

## איך לעשות:
```Lua
-- צירוף באמצעות אופרטור הקונקטנציה '..'
local greeting = "שלום"
local name = "עולם"
local message = greeting .. ", " .. name .. "!"
print(message)  -- הדפסה: שלום, עולם!

-- צירוף באמצעות פונקציית string.format (מומלץ למחרוזות מורכבות)
local age = 30
local formatted = string.format("%s, אתה בן %d שנים.", name, age)
print(formatted) -- הדפסה: עולם, אתה בן 30 שנים.
```

## צלילה עמוקה
הצירוף היה חלק מ-Lua מהרגע הראשון. בשנים הראשונות, צירוף מחרוזות היה פחות יעיל, אך לאורך השנים הופך להיות ביצועית. חלופות לצירוף מחרוזות כוללות את השימוש בטבלאות עם `table.concat`, המתאימה יותר לצירוף מחרוזת המורכבת מחלקים רבים.

פרטי יישום שימושיים:
- ב-Lua, צירוף מחרוזות עלול להיות פעולה יקרה בזמן ריצה אם תעשה בלולאה או בצורה חוזרת.
- אופטימיזציה יכולה להגיע משימוש ב-`table.concat` או בפורמטירה מראש של המחרוזות.
- אם צריך לשלב מחרוזות ומשתנים בצורה מורכבת, `string.format` היא הדרך ללכת אליה עם קוד נקי וקריא.

## ראה גם
- [תיעוד ה-Lua על מחרוזות](https://www.lua.org/manual/5.4/manual.html#6.4)
- [מדריך לימוד Lua](http://tylerneylon.com/a/learn-lua/)
