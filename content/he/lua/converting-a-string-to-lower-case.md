---
title:                "המרת מחרוזת לאותיות קטנות"
html_title:           "Go: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/lua/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## מה ולמה?
המרת מחרוזת לאותיות קטנות היא התהליך שבו אנו משנים את כל האותיות הגדולות במחרוזת לאותיות קטנות. מתכנתים עושים זאת בעיקר כדי לדמות מידע, לשם טיפול פשוט ואחיד בנתונים.

## איך לעשות:
ראשית, ניצור את המחרוזת המקורית:

```Lua
local originalString = "HELLO WORLD!"
```

כעת, נשתמש בפונקציה string.lower() להמרת המחרודת לאותיות קטנות:

```Lua
local lowerCaseString = string.lower(originalString)
```

חלוף זה יפקיד את המחרוזת "hello world!".

## צלילה עמוקה:
המרת מחרוזות לאותיות קטנות הייתה תמיד חלק משפות תכנות. בשפת Lua, string.lower() מבצעת את המשימה הזו, ומשתמשת בטבלת ASCII למעבר בין אותיות גדולות לאותיות קטנות.

הרבה שפות תכנות אחרות, כמו JavaScript או Python, מוכרות שיטות מערך לביצוע פונקציות, כולל המרה לאותיות קטנות. Lua אינה עושה זאת, היא ממקמת פונקציה נפרדת לכל מחרוזת שאנו רוצים לעבד.

## ראה גם:
[Lua 5.4 תיעוד מרכזי](https://www.lua.org/manual/5.4/) - מקור מדהים של מידע על Lua.
[A Lua tutorial](https://www.tutorialspoint.com/lua/index.htm) - מדריך מקיף ללומדי Lua החדשים.