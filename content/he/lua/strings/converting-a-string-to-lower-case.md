---
date: 2024-01-20 17:38:46.563344-07:00
description: "\u05DE\u05D4 \u05D6\u05D4 \u05D4\u05DE\u05E8\u05EA \u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05EA \u05DC\u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\
  \u05D5\u05EA? \u05D6\u05D4 \u05E4\u05E9\u05D5\u05D8 \u05E4\u05E2\u05D5\u05DC\u05D4\
  \ \u05E9\u05D1\u05D4 \u05EA\u05D5\u05D5\u05D9\u05DD \u05D2\u05D3\u05D5\u05DC\u05D9\
  \u05DD \u05D1\u05EA\u05D5\u05DA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DE\u05D5\
  \u05DE\u05E8\u05D9\u05DD \u05DC\u05E7\u05D8\u05E0\u05D9\u05DD. \u05DC\u05DE\u05D4\
  \ \u05D6\u05D4 \u05E0\u05E2\u05E9\u05D4? \u05DB\u05D3\u05D9 \u05DC\u05D0\u05D7\u05D3\
  \ \u05E4\u05D5\u05E8\u05DE\u05D8\u05D9\u05DD, \u05DC\u05E9\u05E4\u05E8 \u05EA\u05D0\
  \u05D9\u05DE\u05D5\u05EA \u05D5\u05DC\u05D4\u05E9\u05D5\u05D5\u05EA \u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05D5\u05EA \u05D1\u05DC\u05D9\u2026"
lastmod: '2024-03-11T00:14:13.003360-06:00'
model: gpt-4-1106-preview
summary: "\u05DE\u05D4 \u05D6\u05D4 \u05D4\u05DE\u05E8\u05EA \u05DE\u05D7\u05E8\u05D5\
  \u05D6\u05EA \u05DC\u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\
  \u05EA? \u05D6\u05D4 \u05E4\u05E9\u05D5\u05D8 \u05E4\u05E2\u05D5\u05DC\u05D4 \u05E9\
  \u05D1\u05D4 \u05EA\u05D5\u05D5\u05D9\u05DD \u05D2\u05D3\u05D5\u05DC\u05D9\u05DD\
  \ \u05D1\u05EA\u05D5\u05DA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DE\u05D5\u05DE\
  \u05E8\u05D9\u05DD \u05DC\u05E7\u05D8\u05E0\u05D9\u05DD. \u05DC\u05DE\u05D4 \u05D6\
  \u05D4 \u05E0\u05E2\u05E9\u05D4? \u05DB\u05D3\u05D9 \u05DC\u05D0\u05D7\u05D3 \u05E4\
  \u05D5\u05E8\u05DE\u05D8\u05D9\u05DD, \u05DC\u05E9\u05E4\u05E8 \u05EA\u05D0\u05D9\
  \u05DE\u05D5\u05EA \u05D5\u05DC\u05D4\u05E9\u05D5\u05D5\u05EA \u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05D5\u05EA \u05D1\u05DC\u05D9\u2026"
title: "\u05D4\u05DE\u05E8\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\u05D0\
  \u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA"
---

{{< edit_this_page >}}

## What & Why?
מה זה המרת מחרוזת לאותיות קטנות? זה פשוט פעולה שבה תווים גדולים בתוך מחרוזת מומרים לקטנים. למה זה נעשה? כדי לאחד פורמטים, לשפר תאימות ולהשוות מחרוזות בלי לדאוג לרישיות.

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
