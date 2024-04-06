---
date: 2024-01-26 03:46:03.704471-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1-Lua \u05D0\
  \u05D9\u05DF \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D9\u05EA \u05E2\u05D9\u05D2\
  \u05D5\u05DC \u05DE\u05D5\u05D1\u05E0\u05D9\u05EA \u05DB\u05DE\u05D5 \u05D1\u05E9\
  \u05E4\u05D5\u05EA \u05D0\u05D7\u05E8\u05D5\u05EA. \u05DE\u05E1\u05D5\u05E8\u05EA\
  \u05D9\u05EA, \u05E2\u05DC\u05D9\u05DA \u05DC\u05DB\u05EA\u05D5\u05D1 \u05D0\u05D7\
  \u05EA \u05DE\u05E9\u05DC\u05DA \u05D0\u05D5 \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9\
  \ \u05D1\u05E1\u05E4\u05E8\u05D9\u05D9\u05D4 \u05E6\u05D3 \u05E9\u05DC\u05D9\u05E9\
  \u05D9. \u05E4\u05EA\u05E8\u05D5\u05E0\u05D5\u05EA \u05E0\u05E4\u05D5\u05E6\u05D9\
  \u05DD \u05DE\u05EA\u05D1\u05E1\u05E1\u05D9\u05DD \u05E2\u05DC\u2026"
lastmod: '2024-04-05T21:53:40.681194-06:00'
model: gpt-4-0125-preview
summary: "\u05D1-Lua \u05D0\u05D9\u05DF \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D9\
  \u05EA \u05E2\u05D9\u05D2\u05D5\u05DC \u05DE\u05D5\u05D1\u05E0\u05D9\u05EA \u05DB\
  \u05DE\u05D5 \u05D1\u05E9\u05E4\u05D5\u05EA \u05D0\u05D7\u05E8\u05D5\u05EA."
title: "\u05E2\u05D9\u05D2\u05D5\u05DC \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD"
weight: 13
---

## איך לעשות:
```lua
-- עיגול בסיסי ב-Lua אינו מובנה, אך ניתן להגדיר פונקציה:

function round(num)
    return num >= 0 and math.floor(num + 0.5) or math.ceil(num - 0.5)
end

print(round(3.5))  -- 4
print(round(2.3))  -- 2
print(round(-1.6)) -- -2

-- לעיגול לנקודה עשרונית מסוימת:
function round(num, decimalPlaces)
    local mult = 10^(decimalPlaces or 0)
    return math.floor(num * mult + 0.5) / mult
end

print(round(3.14159, 2)) -- 3.14
print(round(1.98765, 3))  -- 1.988
```

## צלילה עמוקה
ב-Lua אין פונקציית עיגול מובנית כמו בשפות אחרות. מסורתית, עליך לכתוב אחת משלך או להשתמש בספרייה צד שלישי. פתרונות נפוצים מתבססים על `math.floor()` לעיגול למטה ו-`math.ceil()` לעיגול למעלה, בשילוב עם הוספה או חיסור של 0.5 לפני כן, בהתאם לסימן המספר.

אלטרנטיבות ליצירת פונקציה משלך כוללות ספריות כמו "lua-users wiki" או "Penlight". לכל אחת מהן יתרונות וחסרונות, כמו תכונות נוספות או עומס רב יותר.

באופן פנימי, פונקציות אלו עובדות בדרך כלל על ידי ניצול האופן שבו מחשבים אחסנים מספרים עשרוניים צפים. הוספת 0.5 למספר חיובי שאתה רוצה לעגל ידחוף אותו מעל הסף של הערך השלם הבא, כך שכאשר תיישם `math.floor()` הוא יעגל למטה לשלם הקרוב ביותר.

## ראה גם
- [Lua 5.4 מדריך ייחוס: הפונקציות המתמטיות](https://www.lua.org/manual/5.4/manual.html#6.7)
- [Penlight Lua Libraries: Math](https://github.com/lunarmodules/Penlight)
