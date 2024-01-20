---
title:                "מציאת אורך המחרוזת"
html_title:           "Elm: מציאת אורך המחרוזת"
simple_title:         "מציאת אורך המחרוזת"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/lua/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

##מה ולמה?
חיפוש אחר אורך המחרוזת הוא פרקטיקה בה אנו מחשבים את מספר התווים במחרוזת. מתכנתים ישימים שימוש בכך לשמירה על ציידנות בבחינת, עיבוד מידע לדוג' מחרוזות משתמשים.

##איך:
```Lua
s = "אני אוהב לכתוב ב-Lua"
print(#s)
```
הפלט הצפוי:
```lua
22
```
בשפת Lua, אנחנו משתמשים במשתנה '#' למציאת אורך מחרוזת.

##דיוק עמוק
הפונקציה למציאת אורך נקראת "len" הוצגה לראשונה في Lua 5.1. יתר על כן, בנוסף לשימוש ב'#', אנו יכולים גם להשתמש בפונקציה 'string.len'. הן מחזירות כמות התווים במחרוזת, כולל רווחים וסימנים מיוחדים. 
זו תחליף של "strlen" של C. גם פונקציה זו מספרת תווים באופן ישר.
```Lua
print(string.len("אני אוהב לכתוב ב-Lua"))
```
פלט מצופה:
```Lua
22
```

##ראה גם
עיין במסמכים הבאים ללמידה נוספת:
1. [Lua מאמר מרכזי](https://www.lua.org/pil/11.1.html) 
2. [דוקומנטציה רשמית](https://www.lua.org/manual/5.3/manual.html#pdf-string.len)