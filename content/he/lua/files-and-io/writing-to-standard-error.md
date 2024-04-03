---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:16.514510-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1\u05DC\u05D5\
  \u05D0\u05D4, \u05E0\u05D9\u05EA\u05DF \u05DC\u05DB\u05EA\u05D5\u05D1 \u05DC-stderr\
  \ \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05D4\u05E4\u05D5\u05E0\u05E7\u05E6\
  \u05D9\u05D4 `io.stderr:write()`. \u05D4\u05E0\u05D4 \u05D3\u05D5\u05D2\u05DE\u05D4\
  \ \u05D0\u05D9\u05DA \u05D0\u05E4\u05E9\u05E8 \u05DC\u05DB\u05EA\u05D5\u05D1 \u05D4\
  \u05D5\u05D3\u05E2\u05EA \u05E9\u05D2\u05D9\u05D0\u05D4 \u05E4\u05E9\u05D5\u05D8\
  \u05D4 \u05DC\u05E9\u05D2\u05D9\u05D0\u05D4 \u05E1\u05D8\u05E0\u05D3\u05E8\u05D8\
  \u05D9\u05EA."
lastmod: '2024-03-13T22:44:39.578171-06:00'
model: gpt-4-0125-preview
summary: "\u05D1\u05DC\u05D5\u05D0\u05D4, \u05E0\u05D9\u05EA\u05DF \u05DC\u05DB\u05EA\
  \u05D5\u05D1 \u05DC-stderr \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05D4\u05E4\
  \u05D5\u05E0\u05E7\u05E6\u05D9\u05D4 `io.stderr:write()`."
title: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\u05E9\u05D2\u05D9\u05D0\u05D4 \u05D4\
  \u05EA\u05E7\u05E0\u05D9\u05EA"
weight: 25
---

## איך לעשות:
בלואה, ניתן לכתוב ל-stderr באמצעות הפונקציה `io.stderr:write()`. הנה דוגמה איך אפשר לכתוב הודעת שגיאה פשוטה לשגיאה סטנדרטית:

```lua
io.stderr:write("Error: Invalid input.\n")
```

אם אתם צריכים להוציא פלט של משתנה או לשלב כמה חתיכות נתונים, תשלבו אותם בתוך הפונקציה write:

```lua
local errorMessage = "Invalid input."
io.stderr:write("Error: " .. errorMessage .. "\n")
```

**דוגמת פלט ב-stderr:**
```
Error: Invalid input.
```

עבור תרחישים מורכבים יותר, או כאשר עובדים עם אפליקציות גדולות יותר, ייתכן שתרצו לשקול להשתמש בספריות לוגינג של צד שלישי כמו LuaLogging. עם LuaLogging, אתם יכולים להכוון יומנים ליעדים שונים, כולל stderr. הנה דוגמה קצרה:

תחילה, וודאו ש-LuaLogging מותקן באמצעות LuaRocks:

```
luarocks install lualogging
```

לאחר מכן, כדי לכתוב הודעת שגיאה ל-stderr באמצעות LuaLogging:

```lua
local logging = require("logging")
local logger = logging.stderr()
logger:error("Error: Invalid input.")
```

הגישה הזו מציעה את היתרון של לוגינג מתוקנן ברחבי האפליקציה שלכם, עם הגמישות הנוספת של הגדרת רמות לוג (למשל, ERROR, WARN, INFO) דרך API פשוטה.
