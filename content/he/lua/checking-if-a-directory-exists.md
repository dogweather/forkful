---
title:                "בדיקה האם תיקייה קיימת"
date:                  2024-01-20T14:57:41.662682-07:00
simple_title:         "בדיקה האם תיקייה קיימת"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/lua/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## מה ולמה?
בדיקה אם תיקייה קיימת זהו שלב בו אנחנו וודאים שתיקייה שאנחנו צריכים היא באמת שם לפני שאנחנו נכנסים לעבוד איתה. זה חשוב כי זה מונע שגיאות ומאפשר לנו לעבוד עם הקבצים בצורה נכונה.

## איך לעשות:
ב-Lua, אין פונקציה ישירה בשפה עצמה שבודקת קיום של תיקייה, אבל אפשר להשתמש בממשק של מערכת הפעלה כדי לעשות זאת. הדוגמה הבאה תראה איך לברר אם תיקייה קיימת באמצעות הפעלת פקודת `lfs`.

```Lua
local lfs = require('lfs')

function isDirectoryExists(path)
    local cd = lfs.currentdir()
    local isExists = lfs.chdir(path) and true or false
    lfs.chdir(cd)
    return isExists
end

local dirPath = "/path/to/directory"

if isDirectoryExists(dirPath) then
    print("Directory exists: " .. dirPath)
else
    print("Directory does NOT exist: " .. dirPath)
end
```

פלט עשוי להיות:
```
Directory exists: /path/to/directory
```
או
```
Directory does NOT exist: /path/to/directory
```

## עומק הצלילה
בעבר, אנחנו היינו נאלצים לעשות שימוש בפקודות עם `os.execute` או `io.popen` ולפרסר את התוצאות בעצמנו, מה שיכל להוביל לפרצות אבטחה או נפילות לא צפויות. הספרייה `LuaFileSystem` (lfs) פותחה כדי להפוך עבודה עם מערכות קבצים לנוחה ובטוחה יותר. קיומה מאפשר למפתחים גישה משופרת לפעולות על תיקיות וקבצים. כמובן, תמיכת הפלטפורמה חשובה, ולכן עליכם לוודא שהסביבה בה אתם מריצים את הקוד תומכת ב`lfs`.

השימוש ב`lfs.chdir()` לבדוק אם אפשר להחליף תיקייה זהו טריק חכם. אם הפעולה מצליחה, זה אומר שהתיקייה קיימת. אם לא, אז היא לא קיימת. זיכרו לשחזר את התיקייה הנוכחית בסוף כדי שהצעדים הבאים של התוכנית יוכלו להמשיך כרגיל.

## ראו גם
- LuaFileSystem (LFS): http://keplerproject.github.io/luafilesystem/
- מדריך Lua: https://www.lua.org/manual/5.4/
- איך לבדוק אם קובץ קיים ב-Lua: https://stackoverflow.com/questions/4990990/lua-check-if-a-file-exists
- התיעוד של Lua: https://www.lua.org/docs.html
