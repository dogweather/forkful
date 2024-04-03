---
date: 2024-01-20 17:55:12.905706-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1\u05D5\u05D0\
  \u05D5 \u05E0\u05E8\u05D0\u05D4 \u05D0\u05D9\u05DA \u05DC\u05E7\u05E8\u05D5\u05D0\
  \ \u05DE\u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\u05D8 \u05D1\u05DC\u05D5\u05D0\
  \u05D4."
lastmod: '2024-03-13T22:44:39.579799-06:00'
model: gpt-4-1106-preview
summary: "\u05D1\u05D5\u05D0\u05D5 \u05E0\u05E8\u05D0\u05D4 \u05D0\u05D9\u05DA \u05DC\
  \u05E7\u05E8\u05D5\u05D0 \u05DE\u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\u05D8\
  \ \u05D1\u05DC\u05D5\u05D0\u05D4."
title: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8"
weight: 22
---

## איך לעשות:
בואו נראה איך לקרוא מקובץ טקסט בלואה:

```Lua
-- פתיחת הקובץ לקריאה
local file = io.open("example.txt", "r") -- נניח שיש לנו קובץ בשם example.txt

-- בדיקה האם הקובץ נפתח בהצלחה
if not file then
    error("Failed to open file.")
end

-- קריאת תוכן הקובץ
local content = file:read("*a") -- קוראים את כל התוכן

-- סגירת הקובץ
file:close()

-- הדפסת תוכן הקובץ למסך
print(content)
```

אם בקובץ `example.txt` יש את הטקסט "Hello, Lua!", הפלט יהיה:
```
Hello, Lua!
```

## עומק ים:
בראשית ימי המחשבים, כל הנתונים היו מאוחסנים בקבצים פיזיים. קריאת קבצים היא בסיס לאינטראקציה עם נתונים מחוץ לתוכנית. בלואה, `io.open` יוצר אובייקט "קובץ" ומאפשר לבצע עליו פעולות של קריאה וכתיבה. כאשר עובדים עם קבצים גדולים, קוראים אותם לעיתים בקטעים קטנים (`*l` לקריאת שורה אחת, למשל) כדי לחסוך בזיכרון. חלופה לקריאת קובץ טקסט היא להשתמש בספריות אחרות, כמו `lfs` (Lua File System) לפעולות מתקדמות יותר על קבצים ותיקיות.

## ראו גם:
- [Lua 5.4 Reference Manual (io library)](https://www.lua.org/manual/5.4/manual.html#6.8): מסמך העזר של לואה עם פרטים על ספריית ה-`io`.
- [Programming in Lua (file I/O)](https://www.lua.org/pil/21.1.html): פרק מהספר "תכנות בלואה" המווסת את שימושיות קובץ קלט/פלט.
