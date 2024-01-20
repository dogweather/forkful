---
title:                "חיבור מחרוזות"
html_title:           "C++: חיבור מחרוזות"
simple_title:         "חיבור מחרוזות"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/lua/concatenating-strings.md"
---

{{< edit_this_page >}}

## מה זה ולמה?: Concatenating Strings ערבוב מחרוזות
הצפה של מחרוזות ב-Lua (concatenating) היא תהליך של איחוד שניים או יותר מחרוזות למחרוזת אחת. התכנתים עושים את זה כדי ליצור או לשנות טקסט בצורה גמישה וברורה.

## איך לעשות:
ב-Lua, אנחנו משתמשים בסימן `..` כדי לערבב מחרוזות. נראה איך זה עובד:

```Lua
greeting = "שלום, " 
name = "דוד"
message = greeting .. name
print(message)
```

תוצאה:

```Lua
שלום, דוד
```

## צלילה עמוקה
1. *קונטקסט היסטורי*: בגרסאות הראשונות של Lua, הפונקציה format של string (כמו printf ב-C) הייתה הדרך היחידה לconcentrate מחרוזות.
2. *חלופות*: Lua מציע גם אפשרויות אחרות, כמו string.format, נגיד:
```Lua
name = "דוד"
message = string.format("שלום, %s", name)
print(message)
```
3. *פרטי יישום*: המחרוזת שנוצרת מתהליך הערבוב נמצאת במיקום חדש בזיכרון. המחרוזות המקוריות לא משתנות בגלל שהן אי־ניתנות לשינוי (immutable).

## ראה גם:
- Lua Users Wiki: String Library Tutorial - http://lua-users.org/wiki/StringLibraryTutorial
- Lua 5.1 Reference Manual: String Manipulation - https://www.lua.org/manual/5.1/manual.html#5.4