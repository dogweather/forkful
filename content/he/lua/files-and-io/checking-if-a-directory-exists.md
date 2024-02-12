---
title:                "בדיקה אם ספרייה קיימת"
aliases:
- /he/lua/checking-if-a-directory-exists/
date:                  2024-02-03T19:08:06.337391-07:00
model:                 gpt-4-0125-preview
simple_title:         "בדיקה אם ספרייה קיימת"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/lua/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
