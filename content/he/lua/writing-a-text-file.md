---
title:                "כתיבה לקובץ טקסט"
html_title:           "Bash: כתיבה לקובץ טקסט"
simple_title:         "כתיבה לקובץ טקסט"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/lua/writing-a-text-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבה לקובץ טקסט היא יצירה או עדכון של קובץ עם טקסט. תכניתנים עושים זאת לשמירת נתונים, לוגים, או פלט שהתכנית מייצרת.

## איך לעשות:
```Lua
-- יצירת קובץ וכתיבה
local file = io.open("example.txt", "w")  -- פותחים קובץ לכתיבה
file:write("שלום, עולם!\n")            -- כותבים לקובץ
file:write("נעים מאוד.\n")
file:close()                            -- שומרים וסוגרים את הקובץ

-- קריאה מהקובץ
local file = io.open("example.txt", "r")  -- פותחים קובץ לקריאה
local content = file:read("*a")            -- קוראים את כל התוכן
print(content)                             -- מדפיסים את התוכן
file:close()                               -- סוגרים את הקובץ
```
פלט דוגמא:
```
שלום, עולם!
נעים מאוד.
```

## צלילה עמוקה:
כתיבה לקבצים ב-Lua היא חלק מהסטנדרט של השפה מאז גרסה 5.1. יש לבחור בזהירות את מצב הפתיחה (`"w"`, `"a"`, `"r+"` וכו') כדי למנוע דריסת קבצים קיימים או אובדן נתונים. חלופות כוללות שימוש בספריות חיצוניות לעבודה מתקדמת יותר עם קבצים, כמו למשל LFS (Lua File System).

## ראה גם:
- [Lua 5.4 מדריך רשמי](https://www.lua.org/manual/5.4/)
- [Lua - עבודה עם קבצים](https://www.tutorialspoint.com/lua/lua_file_io.htm)