---
title:                "קריאת קובץ טקסט"
html_title:           "Go: קריאת קובץ טקסט"
simple_title:         "קריאת קובץ טקסט"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/lua/reading-a-text-file.md"
---

{{< edit_this_page >}}

## מה & למה?
קריאה של קובץ טקסט הייא פעולה של הכנסת הנתונים שמשמנים בקובץ לתכנית. מתכנתים מבצעים פעולה זו על מנת לעבד את המידע מראש, או לקלוט נתונים ממשתמשים.

## איך לעשות:
```Lua
-- פתיחת הקובץ לקריאה
file = io.open("test.txt", "r")

-- קריאת כל התוכן 
content = file:read("*a")
print(content)

-- סגירת הקובץ
file:close()
```
הקוד משתמש במודול io של Lua לפתיחה, קריאה וסגירה של קבצים. את המידע שקראנו מקובץ הטקסט נשמר במשתנה content.

## צלילה עמוקה
לגרסאות המוקדמות של Lua לא היה מודול io, אז הגרסה הנוכחית של המודול משקפת תרבות של שיפור והרחבה מדוברות. חלופות למודול io כוללות שימוש בשפות שונות או בספריות Lua שונות עם יכולת של ניהול קבצים. בנוגע לפרטים של היישום, Lua מתרגם שיחות ברמת המערכת שמתבצעות במודול io לשפת המחשב הרלוונטית, המאפשרת עבודה עם קבצים ברמת המערכת.

## ראה גם
- [תיעוד הפעולות של מודול ה-io ב-Lua](http://www.lua.org/manual/5.3/manual.html#6.8)
- [פורום התמיכה של Lua](https://www.lua.org/community.html)
- [Stack Overflow פוסטים שקשורים לטיפול בקבצים ב-Lua](https://stackoverflow.com/questions/tagged/lua+file-io)