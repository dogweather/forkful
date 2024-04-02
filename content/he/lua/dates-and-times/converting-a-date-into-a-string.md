---
date: 2024-01-20 17:37:24.357009-07:00
description: "\u05D4\u05DE\u05E8\u05EA \u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05D4\u05D9\u05D0 \u05E4\u05E9\u05D5\u05D8 \u05DC\
  \u05D9\u05E6\u05D5\u05E8 \u05D9\u05D9\u05E6\u05D5\u05D2 \u05E9\u05DC \u05EA\u05D0\
  \u05E8\u05D9\u05DA \u05DB\u05D8\u05E7\u05E1\u05D8. \u05EA\u05DB\u05E0\u05D9\u05EA\
  \u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\
  \u05D3\u05D9 \u05DC\u05D4\u05E6\u05D9\u05D2 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD\
  \ \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD \u05D1\u05E6\u05D5\u05E8\u05D4 \u05E7\
  \u05E8\u05D9\u05D0\u05D4 \u05DC\u05DE\u05E9\u05EA\u05DE\u05E9 \u05D0\u05D5 \u05DC\
  \u05E9\u05DE\u05D5\u05E8 \u05E4\u05D5\u05E8\u05DE\u05D8 \u05E2\u05E7\u05D1\u05D9\
  \ \u05DC\u05D3\u05D0\u05D8\u05D4 \u05DC\u05D5\u05D2\u05D9\u05E0\u05D2."
lastmod: '2024-03-13T22:44:39.570011-06:00'
model: gpt-4-1106-preview
summary: "\u05D4\u05DE\u05E8\u05EA \u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA \u05D4\u05D9\u05D0 \u05E4\u05E9\u05D5\u05D8 \u05DC\u05D9\
  \u05E6\u05D5\u05E8 \u05D9\u05D9\u05E6\u05D5\u05D2 \u05E9\u05DC \u05EA\u05D0\u05E8\
  \u05D9\u05DA \u05DB\u05D8\u05E7\u05E1\u05D8. \u05EA\u05DB\u05E0\u05D9\u05EA\u05E0\
  \u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\u05D3\
  \u05D9 \u05DC\u05D4\u05E6\u05D9\u05D2 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05EA\
  \u05D0\u05E8\u05D9\u05DB\u05D9\u05DD \u05D1\u05E6\u05D5\u05E8\u05D4 \u05E7\u05E8\
  \u05D9\u05D0\u05D4 \u05DC\u05DE\u05E9\u05EA\u05DE\u05E9 \u05D0\u05D5 \u05DC\u05E9\
  \u05DE\u05D5\u05E8 \u05E4\u05D5\u05E8\u05DE\u05D8 \u05E2\u05E7\u05D1\u05D9 \u05DC\
  \u05D3\u05D0\u05D8\u05D4 \u05DC\u05D5\u05D2\u05D9\u05E0\u05D2."
title: "\u05D4\u05DE\u05E8\u05EA \u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA"
weight: 28
---

## מה ולמה?
המרת תאריך למחרוזת היא פשוט ליצור ייצוג של תאריך כטקסט. תכניתנים עושים את זה כדי להציג נתונים תאריכים בצורה קריאה למשתמש או לשמור פורמט עקבי לדאטה לוגינג.

## איך לעשות:
```Lua
-- המרת תאריך נוכחי למחרוזת
local current_time = os.date("*t") -- מקבל את הזמן הנוכחי
local date_string = string.format("%02d/%02d/%04d", current_time.day, current_time.month, current_time.year)

print(date_string)
-- פלט דוגמה: 31/12/2023
```

```Lua
-- המרת תאריך עם שעה למחרוזת
local date_time_string = os.date("%d/%m/%Y %H:%M:%S", os.time())
print(date_time_string)
-- פלט דוגמה: 31/12/2023 23:59:59
```

## עיון מעמיק
ב-Lua, הפונקציה `os.date` משמשת להמרת זמן למחרוזת. היא ההמשך של סטנדרט POSIX לפונקציית C 'strftime', אבל בעלת סגנון כתיבה של Lua. אפשרויות פורמט מאפשרות שליטה מדויקת על הפלט. בנוסף ל`os.date`, יש פונקציות כמו `os.time` ו`os.clock` לטיפול בתאריכים ובזמנים. 
  
השימוש במחרוזות לתאריך ושעה זוכה לפופולריות כיוון שהוא קריא יותר לאדם ובפורמטים קבועים - נוח לשמירה ולעיבוד עתידי. זהירות, יש תרבויות שונות עם פורמטים שונים לתאריך ושעה, כך שפורמט מחרוזת יכול לדרוש התאמה לוקאלי ספציפי.

## ראו גם
- [Lua 5.4 Reference Manual - os.date](https://www.lua.org/manual/5.4/manual.html#pdf-os.date)
- [Programming in Lua (first edition) - Date and Time](https://www.lua.org/pil/22.1.html)
