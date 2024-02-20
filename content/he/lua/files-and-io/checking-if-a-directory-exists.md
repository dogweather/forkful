---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:06.337391-07:00
description: "\u05D1\u05D3\u05D9\u05E7\u05D4 \u05D0\u05DD \u05EA\u05D9\u05E7\u05D9\
  \u05D9\u05D4 \u05E7\u05D9\u05D9\u05DE\u05EA \u05D4\u05D9\u05D0 \u05E4\u05E2\u05D5\
  \u05DC\u05D4 \u05D9\u05E1\u05D5\u05D3\u05D9\u05EA \u05D1\u05DB\u05EA\u05D9\u05D1\
  \u05EA \u05E1\u05E7\u05E8\u05D9\u05E4\u05D8\u05D9\u05DD \u05D4\u05DE\u05EA\u05E7\
  \u05E9\u05E8\u05D9\u05DD \u05E2\u05DD \u05DE\u05E2\u05E8\u05DB\u05EA \u05D4\u05E7\
  \u05D1\u05E6\u05D9\u05DD, \u05DE\u05D1\u05D8\u05D9\u05D7\u05D4 \u05E9\u05D4\u05EA\
  \u05D5\u05DB\u05E0\u05D9\u05EA \u05E9\u05DC\u05DA \u05E4\u05D5\u05E2\u05DC\u05EA\
  \ \u05E2\u05DC \u05E0\u05EA\u05D9\u05D1\u05D9\u05DD \u05EA\u05E7\u05E4\u05D9\u05DD\
  \ \u05D5\u05DE\u05D5\u05E0\u05E2\u05EA \u05E9\u05D2\u05D9\u05D0\u05D5\u05EA \u05D4\
  \u05E7\u05E9\u05D5\u05E8\u05D5\u05EA \u05DC\u05EA\u05D9\u05E7\u05D9\u05D5\u05EA\u2026"
lastmod: 2024-02-19 22:04:58.824077
model: gpt-4-0125-preview
summary: "\u05D1\u05D3\u05D9\u05E7\u05D4 \u05D0\u05DD \u05EA\u05D9\u05E7\u05D9\u05D9\
  \u05D4 \u05E7\u05D9\u05D9\u05DE\u05EA \u05D4\u05D9\u05D0 \u05E4\u05E2\u05D5\u05DC\
  \u05D4 \u05D9\u05E1\u05D5\u05D3\u05D9\u05EA \u05D1\u05DB\u05EA\u05D9\u05D1\u05EA\
  \ \u05E1\u05E7\u05E8\u05D9\u05E4\u05D8\u05D9\u05DD \u05D4\u05DE\u05EA\u05E7\u05E9\
  \u05E8\u05D9\u05DD \u05E2\u05DD \u05DE\u05E2\u05E8\u05DB\u05EA \u05D4\u05E7\u05D1\
  \u05E6\u05D9\u05DD, \u05DE\u05D1\u05D8\u05D9\u05D7\u05D4 \u05E9\u05D4\u05EA\u05D5\
  \u05DB\u05E0\u05D9\u05EA \u05E9\u05DC\u05DA \u05E4\u05D5\u05E2\u05DC\u05EA \u05E2\
  \u05DC \u05E0\u05EA\u05D9\u05D1\u05D9\u05DD \u05EA\u05E7\u05E4\u05D9\u05DD \u05D5\
  \u05DE\u05D5\u05E0\u05E2\u05EA \u05E9\u05D2\u05D9\u05D0\u05D5\u05EA \u05D4\u05E7\
  \u05E9\u05D5\u05E8\u05D5\u05EA \u05DC\u05EA\u05D9\u05E7\u05D9\u05D5\u05EA\u2026"
title: "\u05D1\u05D3\u05D9\u05E7\u05D4 \u05D0\u05DD \u05E1\u05E4\u05E8\u05D9\u05D9\
  \u05D4 \u05E7\u05D9\u05D9\u05DE\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?

בדיקה אם תיקייה קיימת היא פעולה יסודית בכתיבת סקריפטים המתקשרים עם מערכת הקבצים, מבטיחה שהתוכנית שלך פועלת על נתיבים תקפים ומונעת שגיאות הקשורות לתיקיות שאינן קיימות. משימה זו קרושיאלית ליצירת קבצים חדשים בתיקיות, קריאה מהן, או ביצוע פעולות ספציפיות לתיקיות בבטחה.

## איך לעשות:

ב-Lua, אין לך פונקציה מובנית לבדוק באופן ישיר אם תיקייה קיימת, ולכן אתה לעיתים קרובות מסתמך על ספריית Lua File System (lfs), ספרייה פופולרית של צד שלישי לפעולות עם קבצים.

ראשית, ודא שהתקנת את Lua File System. אם לא, בדרך כלל אתה יכול להתקין אותה באמצעות LuaRocks:

```sh
luarocks install luafilesystem
```

לאחר מכן, אתה יכול להשתמש בדוגמה הבאה לבדוק אם תיקייה קיימת:

```lua
local lfs = require "lfs"

function directoryExists(directory)
    local attr = lfs.attributes(directory)
    return attr and attr.mode == "directory"
end

-- בדוק אם תיקייה מסוימת קיימת
if directoryExists("/path/to/your/directory") then
    print("התיקייה קיימת.")
else
    print("התיקייה לא קיימת.")
end
```

זה יפיק:

```
התיקייה קיימת.
```

או, אם התיקייה לא קיימת:

```
התיקייה לא קיימת.
```

הגישה הזו משתמשת בפונקציית `lfs.attributes` כדי לקבל את המאפיינים של הנתיב. אם הנתיב קיים ומאפיין ה`mode` שלו הוא `directory`, זה מאשר את קיומה של התיקייה.
