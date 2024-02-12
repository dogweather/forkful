---
title:                "עיגול מספרים"
aliases:
- /he/lua/rounding-numbers/
date:                  2024-01-26T03:46:03.704471-07:00
model:                 gpt-4-0125-preview
simple_title:         "עיגול מספרים"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/lua/rounding-numbers.md"
---

{{< edit_this_page >}}

## מה ולמה?
עיגול מספרים פירושו התאמתם לשלם הקרוב ביותר או לנקודה עשרונית מצוינת. זהו יסוד בתכנות להפחתת מורכבות, שיפור ביצועים, ולמקרים בהם דיוק מעבר לנקודה מסוימת אינו מוסיף ערך.

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
