---
title:                "המרת מחרוזת לאותיות קטנות"
aliases:
- he/lua/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:38:46.563344-07:00
model:                 gpt-4-1106-preview
simple_title:         "המרת מחרוזת לאותיות קטנות"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/lua/converting-a-string-to-lower-case.md"
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
