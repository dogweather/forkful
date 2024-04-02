---
date: 2024-01-20 17:46:05.731657-07:00
description: "\u05DE\u05D4 \u05D6\u05D4 \u05D7\u05DC\u05E7\u05D9 \u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05D5\u05EA (substrings) \u05D5\u05DC\u05DE\u05D4 \u05D0\u05E0\u05D7\
  \u05E0\u05D5 \u05E6\u05E8\u05D9\u05DB\u05D9\u05DD \u05DC\u05D4\u05E9\u05D9\u05D2\
  \ \u05D0\u05D5\u05EA\u05DD? \u05D4\u05D4\u05D5\u05E6\u05D0\u05D4 \u05E9\u05DC \u05D7\
  \u05DC\u05E7\u05D9 \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D6\u05D4 \u05DB\u05D0\
  \u05E9\u05E8 \u05D0\u05E0\u05D5 \u05D1\u05D5\u05D7\u05E8\u05D9\u05DD \u05D7\u05DC\
  \u05E7 \u05DE\u05E1\u05D5\u05D9\u05DD \u05E9\u05DC \u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05EA \u05D5\u05DE\u05D1\u05D5\u05D3\u05D3\u05D9\u05DD \u05D0\u05D5\u05EA\u05D5\
  . \u05EA\u05D5\u05DB\u05E0\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD\
  \u2026"
lastmod: '2024-03-13T22:44:39.532832-06:00'
model: gpt-4-1106-preview
summary: "\u05DE\u05D4 \u05D6\u05D4 \u05D7\u05DC\u05E7\u05D9 \u05DE\u05D7\u05E8\u05D5\
  \u05D6\u05D5\u05EA (substrings) \u05D5\u05DC\u05DE\u05D4 \u05D0\u05E0\u05D7\u05E0\
  \u05D5 \u05E6\u05E8\u05D9\u05DB\u05D9\u05DD \u05DC\u05D4\u05E9\u05D9\u05D2 \u05D0\
  \u05D5\u05EA\u05DD? \u05D4\u05D4\u05D5\u05E6\u05D0\u05D4 \u05E9\u05DC \u05D7\u05DC\
  \u05E7\u05D9 \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D6\u05D4 \u05DB\u05D0\u05E9\
  \u05E8 \u05D0\u05E0\u05D5 \u05D1\u05D5\u05D7\u05E8\u05D9\u05DD \u05D7\u05DC\u05E7\
  \ \u05DE\u05E1\u05D5\u05D9\u05DD \u05E9\u05DC \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA\
  \ \u05D5\u05DE\u05D1\u05D5\u05D3\u05D3\u05D9\u05DD \u05D0\u05D5\u05EA\u05D5. \u05EA\
  \u05D5\u05DB\u05E0\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD\u2026"
title: "\u05D7\u05D9\u05DC\u05D5\u05E5 \u05EA\u05EA-\u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05D5\u05EA"
weight: 6
---

## What & Why?
מה זה חלקי מחרוזות (substrings) ולמה אנחנו צריכים להשיג אותם? ההוצאה של חלקי מחרוזת זה כאשר אנו בוחרים חלק מסוים של מחרוזת ומבודדים אותו. תוכנתנים עושים זאת כדי לנתח נתונים, לולאות (validate) קלטים ולעבד מידע בצורה יעילה.

## How to:
הנה איך מוציאים חלקי מחרוזת ב-Lua:
```lua
-- דוגמא 1: קבלת חלק מהתחלה עם רשימת פרמטרים
local text = "שלום עולם"
local subtext = text:sub(1, 5)
print(subtext)  -- יוצא "שלום"

-- דוגמא 2: קבלת חלק מסוף ללא פרמטר שני
local subtext_end = text:sub(-5)
print(subtext_end)  -- יוצא "עולם"

-- דוגמא 3: קבלת תת-מחרוזת באמצעות תבנית (pattern)
local pattern_subtext = string.match(text, "עול(.*)")
print(pattern_subtext)  -- יוצא "ם"
```

## Deep Dive
הוצאת חלקי מחרוזת היא פונקציה בסיסית שניתן למצוא ברוב שפות התכנות. ב-Lua, הפונקציה `sub` מופיעה בגרסה 5 ומעלה. יש גם שיטות אלטרנטיביות כמו שימוש בביטויים רגולריים באמצעות המודול 'string'. היעילות של ההוצאה תלויה בגודל המחרוזת ובפונקציה שבחרת להשתמש.

## See Also
- התיעוד הרשמי של Lua לעבודה עם מחרוזות: http://www.lua.org/manual/5.4/manual.html#6.4
- מדריך לשימוש בביטויים רגולריים ב-Lua: https://www.lua.org/pil/20.2.html
- פורום Stack Overflow של Lua לשאלות נפוצות: https://stackoverflow.com/questions/tagged/lua
