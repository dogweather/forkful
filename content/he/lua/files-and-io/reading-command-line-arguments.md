---
date: 2024-01-20 17:56:37.814860-07:00
description: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05D0\u05E8\u05D2\u05D5\u05DE\u05E0\u05D8\
  \u05D9\u05DD \u05DE\u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4\
  \ \u05DE\u05D0\u05E4\u05E9\u05E8\u05EA \u05DC\u05EA\u05D5\u05DB\u05E0\u05D5\u05EA\
  \ \u05DC\u05E7\u05D1\u05DC \u05E7\u05DC\u05D8 \u05DE\u05D4\u05DE\u05E9\u05EA\u05DE\
  \u05E9 \u05DE\u05D7\u05D5\u05E5 \u05DC\u05DE\u05DE\u05E9\u05E7 \u05D4\u05D2\u05E8\
  \u05E4\u05D9. \u05EA\u05DB\u05E0\u05D9\u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\
  \u05E9\u05D9\u05DD \u05D1\u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05D4\u05E4\u05E2\
  \u05D9\u05DC \u05D0\u05EA \u05D4\u05EA\u05D5\u05DB\u05E0\u05D4 \u05E2\u05DD \u05D4\
  \u05D2\u05D3\u05E8\u05D5\u05EA \u05DE\u05E1\u05D5\u05D5\u05D2\u05D5\u05EA \u05D0\
  \u05D5 \u05DC\u05E2\u05D1\u05D3 \u05E7\u05DC\u05D8\u2026"
lastmod: '2024-03-13T22:44:39.576582-06:00'
model: gpt-4-1106-preview
summary: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05D0\u05E8\u05D2\u05D5\u05DE\u05E0\u05D8\
  \u05D9\u05DD \u05DE\u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4\
  \ \u05DE\u05D0\u05E4\u05E9\u05E8\u05EA \u05DC\u05EA\u05D5\u05DB\u05E0\u05D5\u05EA\
  \ \u05DC\u05E7\u05D1\u05DC \u05E7\u05DC\u05D8 \u05DE\u05D4\u05DE\u05E9\u05EA\u05DE\
  \u05E9 \u05DE\u05D7\u05D5\u05E5 \u05DC\u05DE\u05DE\u05E9\u05E7 \u05D4\u05D2\u05E8\
  \u05E4\u05D9. \u05EA\u05DB\u05E0\u05D9\u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\
  \u05E9\u05D9\u05DD \u05D1\u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05D4\u05E4\u05E2\
  \u05D9\u05DC \u05D0\u05EA \u05D4\u05EA\u05D5\u05DB\u05E0\u05D4 \u05E2\u05DD \u05D4\
  \u05D2\u05D3\u05E8\u05D5\u05EA \u05DE\u05E1\u05D5\u05D5\u05D2\u05D5\u05EA \u05D0\
  \u05D5 \u05DC\u05E2\u05D1\u05D3 \u05E7\u05DC\u05D8\u2026"
title: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E4\u05E8\u05DE\u05D8\u05E8\u05D9\u05DD\
  \ \u05DE\u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4"
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
