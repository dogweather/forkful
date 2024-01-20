---
title:                "חילוץ תת-מחרוזות"
html_title:           "Bash: חילוץ תת-מחרוזות"
simple_title:         "חילוץ תת-מחרוזות"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/lua/extracting-substrings.md"
---

{{< edit_this_page >}}

##מה זה ולמה? 
חילוץ substring הוא הפרדת חלק ממחרוזת עיקרית. מתכנתים משתמשים בזה כדי לנפות או לשנות מידע מתוך מחרוזות באופן יעיל.

##איך עושים זאת:
שיעור מהיר בחילוף substring בשפת Lua. התרגיל הפעם: חלץ את "Lua" מתחת למחרוזת "אני אוהב Lua!"

```Lua
str = "אני אוהב Lua!"
startPos, endPos = string.find(str, "Lua")
Substring = string.sub(str, startPos, endPos)
print(Substring)
``` 

הדפסה:

```
Lua
```

##מעמיקים:
הפונקציה `string.sub` של Lua היא כלי חזק לחילוץ תת-מחרוזות, אך היא לא הייתה תמיד חלק מהפנים האפשריים של השפה. בשלבים מוקדמים של Lua, ניתן היה לביצוע פעולות דומות באמצעות מישורים ותווים.

ניתן אף לחלץ את התת-מחרוזת בלי שימוש ב-`string.sub` , כמו למשל במימוש של משקף-אמצעי Regex, אך טכניקה זו היא בדרך כלל מורכבת יותר.

זכור שב-Lua (כמו בכל מערכת שפועלת בעידן zero-based), הספרה הראשונה היא 1, ולא 0.

##קראו גם:
1. ["Programming in Lua" - מדריך המתחיל](https://www.lua.org/pil/1.html)
2. [חילוץ תת-מחרוזות של Lua - Wiki](https://www.lua.org/pil/20.2.html)
3. ["String Manipulation in Lua" מאת Roberto Ierusalimschy](https://www.lua.org/pil/20.html)