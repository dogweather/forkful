---
date: 2024-01-20 17:38:46.563344-07:00
description: "How to: \u05D4\u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D4 `lower()` \u05D6\
  \u05DE\u05D9\u05E0\u05D4 \u05E2\u05DC \u05DB\u05DC \u05E2\u05E6\u05DD \u05DE\u05E1\
  \u05D5\u05D2 `string`. \u05D6\u05D4 \u05DE\u05DE\u05D9\u05E8 \u05D0\u05EA \u05DB\
  \u05DC \u05D4\u05EA\u05D5\u05D5\u05D9\u05DD \u05D1\u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05EA \u05DC\u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA\
  ."
lastmod: '2024-04-05T21:53:40.671952-06:00'
model: gpt-4-1106-preview
summary: "\u05D4\u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D4 `lower()` \u05D6\u05DE\u05D9\
  \u05E0\u05D4 \u05E2\u05DC \u05DB\u05DC \u05E2\u05E6\u05DD \u05DE\u05E1\u05D5\u05D2\
  \ `string`."
title: "\u05D4\u05DE\u05E8\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\u05D0\
  \u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA"
weight: 4
---

## How to:
```Lua
-- מהמרים מחרוזת לאותיות קטנות בלואה

local myString = "Shalom, OLAM!"
local lowerString = myString:lower()

print(lowerString)  -- תוצאה: shalom, olam!
```

הפונקציה `lower()` זמינה על כל עצם מסוג `string`. זה ממיר את כל התווים במחרוזת לאותיות קטנות.

## Deep Dive
מהמרה לאותיות קטנות היא פונקציה סטנדרטית ברוב שפות התכנות. בלואה, `string.lower()` היא הדרך המובנית לעשות זאת. הפונקציה מתבססת על כללי ה-UniCode למה שחשוב להדגיש במיוחד כשעובדים עם שפות כמו עברית שבהן יש תווים מיוחדים. יש כמובן אלטרנטיבות כמו כתיבת פונקציה מותאמת אישית, אבל ברוב המקרים, הפונקציה המובנית תספיק.

## See Also
- Lua 5.4 Reference Manual: [www.lua.org/manual/5.4/manual.html#pdf-string.lower](https://www.lua.org/manual/5.4/manual.html#pdf-string.lower)
- UniCode Standard: [www.unicode.org](https://www.unicode.org/)
