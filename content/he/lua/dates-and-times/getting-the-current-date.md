---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:53.319886-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: Lua \u05DE\u05E1\u05E4\
  \u05E7\u05EA \u05D0\u05EA \u05D4\u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D4 `os.date`\
  \ \u05DC\u05E7\u05D1\u05DC\u05EA \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05D5\u05D4\
  \u05E9\u05E2\u05D4 \u05D4\u05E0\u05D5\u05DB\u05D7\u05D9\u05D9\u05DD. \u05E0\u05D9\
  \u05EA\u05DF \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\u05E4\u05D5\u05E0\u05E7\
  \u05E6\u05D9\u05D4 \u05D6\u05D5 \u05DC\u05DC\u05D0 \u05D0\u05E8\u05D2\u05D5\u05DE\
  \u05E0\u05D8\u05D9\u05DD \u05DB\u05D3\u05D9 \u05DC\u05E7\u05D1\u05DC \u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA \u05DE\u05E2\u05D5\u05E6\u05D1\u05EA \u05D0\u05D5 \u05E2\
  \u05DD \u05DE\u05E4\u05E8\u05D8\u05D9 \u05E4\u05D5\u05E8\u05DE\u05D8 \u05DB\u05D3\
  \u05D9\u2026"
lastmod: '2024-03-13T22:44:39.568320-06:00'
model: gpt-4-0125-preview
summary: "Lua \u05DE\u05E1\u05E4\u05E7\u05EA \u05D0\u05EA \u05D4\u05E4\u05D5\u05E0\
  \u05E7\u05E6\u05D9\u05D4 `os.date` \u05DC\u05E7\u05D1\u05DC\u05EA \u05D4\u05EA\u05D0\
  \u05E8\u05D9\u05DA \u05D5\u05D4\u05E9\u05E2\u05D4 \u05D4\u05E0\u05D5\u05DB\u05D7\
  \u05D9\u05D9\u05DD."
title: "\u05E7\u05D1\u05DC\u05EA \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05D4\u05E0\
  \u05D5\u05DB\u05D7\u05D9"
weight: 29
---

## איך לעשות:
Lua מספקת את הפונקציה `os.date` לקבלת התאריך והשעה הנוכחיים. ניתן להשתמש בפונקציה זו ללא ארגומנטים כדי לקבל מחרוזת מעוצבת או עם מפרטי פורמט כדי להתאים פלט. הנה איך להשתמש בה:

```lua
-- קבלת התאריך והשעה הנוכחיים כמחרוזת מעוצבת
print(os.date())  -- לדוגמה, Thu Mar  3 14:02:03 2022

-- התאמת פורמט הפלט
-- %Y לשנה, %m לחודש, %d ליום, %H לשעה, %M לדקות
print(os.date("%Y-%m-%d %H:%M"))  -- לדוגמה, 2022-03-03 14:02
```

לשם ביצוע מניפולציה יותר מתקדמת של תאריכים ושעות, ל-Lua אין ספריות מובנות מורחבות כמו בחלק משפות התכנות האחרות. עם זאת, ניתן להשתמש בספריות צד שלישי כמו `lua-date` (https://github.com/Tieske/date). ספרייה זו מציעה פונקציונליות יותר מקיפה למניפולציה של תאריכים וזמנים. הנה איך אפשר להשתמש בה:

ראשית, וודא שהתקנת את ספריית `lua-date`. בדרך כלל אפשר להתקין אותה באמצעות LuaRocks עם הפקודה הבאה:

```bash
luarocks install lua-date
```

לאחר מכן, תוכל להשתמש בה בסקריפט של Lua כך:

```lua
local date = require("date")

-- יצירת אובייקט תאריך עבור התאריך והשעה הנוכחיים
local now = date()

print(now:fmt("%Y-%m-%d %H:%M:%S"))  -- לדוגמה, 2022-03-03 14:02:03
```

דוגמה זו מדגימה את יצירת אובייקט `date` המייצג את הרגע הנוכחי, שאותו ניתן לעצב באופן דומה לפונקציה `os.date` אך עם גמישות ואפשרויות נוספות המוצעות על ידי ספריית `lua-date`.
