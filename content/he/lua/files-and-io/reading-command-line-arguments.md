---
date: 2024-01-20 17:56:37.814860-07:00
description: "\u05D0\u05D9\u05DA \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\
  \u05D4: \u05E4\u05DC\u05D8 \u05DC\u05D3\u05D5\u05D2\u05DE\u05D0."
lastmod: '2024-04-05T21:53:40.707906-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E4\u05E8\u05DE\u05D8\u05E8\u05D9\u05DD\
  \ \u05DE\u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4"
weight: 23
---

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
