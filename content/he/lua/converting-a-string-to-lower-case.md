---
title:                "המרת מחרוזת לתווים קטנים"
html_title:           "Lua: המרת מחרוזת לתווים קטנים"
simple_title:         "המרת מחרוזת לתווים קטנים"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/lua/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

כתיבת טקסט באותיות קטנות בלואה

## מה ולמה?

המרת טקסט לאותיות קטנות היא תהליך שבו המחשב משנה את האותיות במחרוזת לפי ההגדרות של אותיות קטנות. סיבה נפוצה לביצוע פעולה זו היא לצורך ייעול וקריאות בקוד, כאשר המון שפות תכנות יש להן הבדלים בין אותיות קטנות לאותיות גדולות.

## איך לעשות זאת:

בלואה ניתן להשתמש בפונקציה `string.lower` כדי להמיר את האותיות של מחרוזת לאותיות קטנות. 

```Lua
local sentence = "שלום לכולם"
print(string.lower(sentence))
```
פלט:
```
שלום לכולם
```

לדוגמה, אם תרצו להדפיס את השם של המשתמש בכתיבת אותיות קטנות, ניתן לעשות זאת כך:

```Lua
print("שם משתמש: " .. string.lower(username))
```

או אם תרצו לבדוק אם מילה מסוימת נמצאת באותיות קטנות, ניתן לעבוד כך:

```Lua
local word = "אורז"
if string.lower(word) == "אורז" then
  print("המילה נמצאת באותיות קטנות")
end
```

## מסע לעומק:

בנוסף לשימוש בפונקציה `string.lower`, ניתן גם להשתמש במפתחות תכנותיים כמו `string.byte` ו-`string.upper` להמיר את האותיות לקודים ולבדוק את גודלם.

יחד עם זאת, כדי להתאים לפניות למסור לאותיות קטנות, כל מחרוזת בלואה בסופו של דבר עוברת דרך פונקציה כדי להבטיח שהמחרוזת תהיה בפורמט הנכון. 

## ראו גם:

התחברו לקישורים הבאים כדי לקרוא עוד על ההמרה לאותיות קטנות בלואה:

- https://www.lua.org/manual/5.1/manual.html#5.4
- https://www.lua.org/manual/5.1/manual.html#5.3.4
- https://www.lua.org/manual/5.1/manual.html#5.4.1