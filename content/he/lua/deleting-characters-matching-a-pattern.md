---
title:                "מחיקת תווים התואמים לתבנית"
html_title:           "Elixir: מחיקת תווים התואמים לתבנית"
simple_title:         "מחיקת תווים התואמים לתבנית"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/lua/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## מה ולמה?

מחיקת תווים המתאימים לדפוס היא פעולה שבה מסירים חלק ממחרוזת, בהתאם לדפוס שהוגדר. מתכנתים משתמשים בכך בעיקר כדי לנקות ולטהר מידע, להכנתו לעיבוד נוסף.

## הדרכה:

קוד לואה כדי למחוק את כל האותיות 'a' ממחרוזת:

```Lua
local str = "bananas"
str = str:gsub('a', '')
print(str) -- Output: "bnns"
```

בדוגמה הזו, gsub הוא הפעולה שמאפשרת לנו לאתר את כל האותיות 'a' במחרוזת ולהסיר אותן.  

## צלילה עמוקה:

Lua, שהווה את הגרסה הנוכחית, מספקת רקע מעניין לנושא. פונקציית ה-gsub הופיעה לראשונה במהדורה RFC 4180 שפורסמה באוקטובר 2005. מאז, היא הפכה להיות אחת מהכלים הבסיסיים של מתכנת לואה לעריכת מחרוזות. בחלק מהשפות, כמו JavaScript, נדרשת שימוש בכלי אחר למחיקת תווים המתאימים לדפוס, אך לואה מספקת ממשק נוח ומהיר בעזרת gsub.

## ראה גם:

למדע נרחב יותר, עיין ב:
1. האקדמיה ללמידת לואה: [https://www.lua.org/start.html](https://www.lua.org/start.html)
2. תיעוד ה-API של Lua: [https://www.lua.org/manual/5.4/](https://www.lua.org/manual/5.4/)
3. ספר המתכנת של Lua: [https://www.lua.org/pil/](https://www.lua.org/pil/)