---
aliases:
- /he/lua/writing-to-standard-error/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:16.514510-07:00
description: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\u05E9\u05D2\u05D9\u05D0\u05D4\
  \ \u05E1\u05D8\u05E0\u05D3\u05E8\u05D8\u05D9\u05EA (stderr) \u05E2\u05D5\u05E1\u05E7\
  \u05EA \u05D1\u05D4\u05DB\u05D5\u05D5\u05E0\u05EA \u05D4\u05D5\u05D3\u05E2\u05D5\
  \u05EA \u05E9\u05D2\u05D9\u05D0\u05D4 \u05D5\u05E4\u05DC\u05D8\u05D9\u05DD \u05D3\
  \u05D9\u05D0\u05D2\u05E0\u05D5\u05E1\u05D8\u05D9\u05D9\u05DD \u05DC\u05E2\u05E8\u05D5\
  \u05E5 \u05E0\u05E4\u05E8\u05D3, \u05D4\u05E0\u05D1\u05D3\u05DC \u05DE\u05D4\u05E4\
  \u05DC\u05D8 \u05D4\u05E1\u05D8\u05E0\u05D3\u05E8\u05D8\u05D9 (stdout). \u05DE\u05EA\
  \u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA\
  \ \u05DB\u05D3\u05D9 \u05DC\u05D4\u05D1\u05D7\u05D9\u05DF \u05D1\u05D9\u05DF\u2026"
lastmod: 2024-02-18 23:08:52.993946
model: gpt-4-0125-preview
summary: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\u05E9\u05D2\u05D9\u05D0\u05D4 \u05E1\
  \u05D8\u05E0\u05D3\u05E8\u05D8\u05D9\u05EA (stderr) \u05E2\u05D5\u05E1\u05E7\u05EA\
  \ \u05D1\u05D4\u05DB\u05D5\u05D5\u05E0\u05EA \u05D4\u05D5\u05D3\u05E2\u05D5\u05EA\
  \ \u05E9\u05D2\u05D9\u05D0\u05D4 \u05D5\u05E4\u05DC\u05D8\u05D9\u05DD \u05D3\u05D9\
  \u05D0\u05D2\u05E0\u05D5\u05E1\u05D8\u05D9\u05D9\u05DD \u05DC\u05E2\u05E8\u05D5\u05E5\
  \ \u05E0\u05E4\u05E8\u05D3, \u05D4\u05E0\u05D1\u05D3\u05DC \u05DE\u05D4\u05E4\u05DC\
  \u05D8 \u05D4\u05E1\u05D8\u05E0\u05D3\u05E8\u05D8\u05D9 (stdout). \u05DE\u05EA\u05DB\
  \u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\
  \u05D3\u05D9 \u05DC\u05D4\u05D1\u05D7\u05D9\u05DF \u05D1\u05D9\u05DF\u2026"
title: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\u05E9\u05D2\u05D9\u05D0\u05D4 \u05D4\
  \u05EA\u05E7\u05E0\u05D9\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבה לשגיאה סטנדרטית (stderr) עוסקת בהכוונת הודעות שגיאה ופלטים דיאגנוסטיים לערוץ נפרד, הנבדל מהפלט הסטנדרטי (stdout). מתכנתים עושים זאת כדי להבחין בין תוצאות התוכנית הרגילות לבין מידע על שגיאות, ובכך מייעלים את תהליכי הניפוי של באגים ותהליכי הלוגינג.

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
