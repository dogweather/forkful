---
title:                "שימוש בביטויים רגולריים"
html_title:           "Bash: שימוש בביטויים רגולריים"
simple_title:         "שימוש בביטויים רגולריים"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/lua/using-regular-expressions.md"
---

{{< edit_this_page >}}

## מה ולמה?
רגולר אקספרשנס (ביטויים רגולריים) הם דרך לחיפוש ועיבוד טקסטים בהתאם לתבניות קבועות. תכניתנים עושים שימוש בהם לצורך ולידציה, חיפוש, והחלפת תוכן באופן אוטומטי ויעיל.

## איך עושים את זה:
להלן דוגמה לאיך להשתמש בביטויים רגולריים בLua:

```Lua
local text = "היי, אני לומד Lua."
local pattern = "%a+"

for word in text:gmatch(pattern) do
    print(word)
end
-- הוצאה:
-- היי
-- אני
-- לומד
-- Lua

-- חיפוש והחלפה
local changed_text = text:gsub("(%a+)", "מילה")
print(changed_text) -- הוצאה: מילה, מילה מילה Lua.
```

## ירידה לעומק
ביטויים רגולריים הם כלי חזק שהשתפר והתפתח במהלך השנים, מאז פותחו בשנות ה-50. בLua, הם מיושמים באופן חלקי ואינם תומכים בכל התכונות הסטנדרטיות של רגולר אקספרשנס, כמו בשפות אחרות. חלופות כוללות שימוש במודולים חיצוניים כגון Lrexlib או luaposix.

## ראה גם
- [Lua Patterns Reference](https://www.lua.org/manual/5.4/manual.html#6.4.1)
- [Lrexlib](https://github.com/rrthomas/lrexlib)
- [Programming in Lua (for Patterns and Matches)](https://www.lua.org/pil/20.2.html)
