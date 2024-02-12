---
title:                "כתיבה לשגיאה התקנית"
aliases: - /he/lua/writing-to-standard-error.md
date:                  2024-02-03T19:34:16.514510-07:00
model:                 gpt-4-0125-preview
simple_title:         "כתיבה לשגיאה התקנית"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/lua/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
