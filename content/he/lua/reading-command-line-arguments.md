---
title:                "קריאת פרמטרים משורת הפקודה"
date:                  2024-01-20T17:56:37.814860-07:00
model:                 gpt-4-1106-preview
simple_title:         "קריאת פרמטרים משורת הפקודה"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/lua/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## מה ולמה?
קריאת ארגומנטים משורת הפקודה מאפשרת לתוכנות לקבל קלט מהמשתמש מחוץ לממשק הגרפי. תכניתים משתמשים בזה כדי להפעיל את התוכנה עם הגדרות מסווגות או לעבד קלט דינמי.

## איך עושים את זה:
```Lua
-- save as cli_args.lua
for i = 1, #arg do
    print("Argument " .. i .. ": " .. arg[i])
end
```

פלט לדוגמא:
```
$ lua cli_args.lua firstArg secondArg
Argument 1: firstArg
Argument 2: secondArg
```

## צלילה עמוקה
ב-Lua, המשתנה הגלובלי `arg` מכיל את ארגומנטי שורת הפקודה. `arg[0]` הוא שם הסקריפט עצמו, ו-`arg[i]` (עבור i גדול מ-0) הם הארגומנטים.

בעבר, נהוג היה להשתמש ב-`table.getn(arg)` לקבלת מספר הארגומנטים, אבל בגרסאות האחרונות של Lua, פונקציה זו אינה קיימת כבר והחליפנו אותה בשימוש ב-`#arg`.

ישנן אלטרנטיבות כמו מודולים שלישיים לפרסור וניתוח ארגומנטים מתקדם, אבל לרוב צרכים פשוטים ומהירים השיטה המוצגת לעיל תעשה את העבודה.

## ראה גם
- [Lua 5.4 Reference Manual - The Command Line](https://www.lua.org/manual/5.4/manual.html#6.1)
- [Programming in Lua (first edition)](https://www.lua.org/pil/22.1.html)
