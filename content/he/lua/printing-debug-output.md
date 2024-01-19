---
title:                "הדפסת פלט ניפוי שגיאות"
html_title:           "Arduino: הדפסת פלט ניפוי שגיאות"
simple_title:         "הדפסת פלט ניפוי שגיאות"
programming_language: "Lua"
category:             "Lua"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/lua/printing-debug-output.md"
---

{{< edit_this_page >}}

## מה זה ולמה?

דיבוג הוא תהליך שבו מתכנת בוחן ומפטר את הבאגים (השגיאות) בתוכנית. אנו משתמשים בהדפסת מידע דיבוג כדי להבין מה קורה בהפעלת התוכנית בזמן אמת, מה יכול להקל על תיקון באגים קיימים ומניעת באגים בעתיד.

## איך לעשות:

```Lua
-- להדפיס את הודעה טקסטואלית
print("האם זה עובד? בוא נראה!")

-- להדפיס את סטרינג עם משתנה בתוכו
name = "John"
print("שלום, " .. name)
```

את ההדפסה במסגרת דיבוג אפשר להשתמש ב:

```Lua
local var = "זה הכל"
print("מידע דיבוג: ", var)
```

## טיפול מעמיק:

הדפסת מידע דיבוג היא השיטה האניטרית ביותר עבור מתכנתים מאז תחילת התכנות. היא תמיד אמינה ופשוטה לשימוש. ישנם גם כלים מתחכמים יותר, אך עיתים קרובות הפתרון הפשוט ביותר הוא הטוב ביותר. 

## ראה גם:

- [מונחי Lua](https://www.lua.org/manual/5.4/manual.html#6.9)
- [קורס Lua בCodecademy](https://www.codecademy.com/learn/learn-lua) 
- [ספר לימוד Lua](https://www.lua.org/pil/)