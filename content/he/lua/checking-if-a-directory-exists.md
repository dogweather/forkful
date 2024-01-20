---
title:                "בדיקה אם ספרייה קיימת"
html_title:           "Java: בדיקה אם ספרייה קיימת"
simple_title:         "בדיקה אם ספרייה קיימת"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/lua/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# בדיקה אם תיקייה קיימת בלואה 

## מה זה ולמה?
בהצלחה לבדוק אם תיקייה קיימת במערכת הקבצים היא חלק חיוני בכתיבת קוד מתקדם. זה מאפשר לנו לבטל את אפשרות השגיאות בתהליך, כמו לנסות לכתוב קובץ לתיקייה שאינה קיימת.

## איך לעשות:

```Lua
local lfs = require('lfs')

-- שם התיקייה לבדיקה
local dir = '/path/to/directoy'

-- פונקציה לבדיקה האם תיקייה קיימת
local function directory_exists(path)
  -- בדיקה באמצעות lfs.attributes
  -- מחזירה nil אם התיקייה לא קיימת 
  return lfs.attributes(path,"mode") == "directory"
end

-- משתמשים בפונקציה
if directory_exists(dir) then
  print(dir.. ' קיימת')
else
  print(dir.. ' לא קיימת')
end
```

## צלילה עמוקה
לואה הייתה מקבלת פונקציה קבועה שאפשרה לבדוק האם תיקייה קיימת, אך בגרסאות החדשות ביותר, הוסיף הקהל את ספריית 'lfs' (Lua File System), שהיא האופציה המקובלת כיום.

אלטרנטיבה ל-lfs היא השימוש ב- os.execute עם הפקודה 'ls' (בלינוקס ומק), או 'dir' (בווינדוס). זה יכול לעבוד, אך לא מומלץ מכיוון שהיא מתלויה במערכת ההפעלה ועלולה להוות סיכון אבטחתי.

לגבי הביצוע, השימוש ב-luafilesystem הוא מהיר ויעיל. הוא מתבצע בצורה ישירה דרך ה-API של המערכת המקומית, ולא יוצר תהליכים חיצוניים או מחכה להחזרות של מערכת ההפעלה.

## ראה גם
רשימה של כמה משאבים שיכולים לעזור לכם להעמיק את הידע שלכם בנושא זה:
- [LuaFileSystem - ממשק קבצי Lua](https://keplerproject.github.io/luafilesystem)
- [Lua 5.1 המדריך למתחילים](http://lua-users.org/wiki/ForTutorial)
- [StackOverflow - בדיקה אם ספרייה קיימת ב-Lua](https://stackoverflow.com/questions/1340230/check-if-directory-exists-in-lua)