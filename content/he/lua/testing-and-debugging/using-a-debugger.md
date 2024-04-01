---
date: 2024-01-26 04:09:45.706766-07:00
description: "\u05D3\u05D9\u05D1\u05D0\u05D2\u05E8 \u05D4\u05D5\u05D0 \u05DB\u05DC\
  \u05D9 \u05E9\u05DE\u05D0\u05E4\u05E9\u05E8 \u05DC\u05DA \u05DC\u05D1\u05D3\u05D5\
  \u05E7 \u05D5\u05DC\u05E9\u05DC\u05D5\u05D8 \u05D1\u05D1\u05D9\u05E6\u05D5\u05E2\
  \ \u05D4\u05EA\u05D5\u05DB\u05E0\u05D9\u05EA, \u05DE\u05D4 \u05E9\u05D4\u05D5\u05E4\
  \u05DA \u05D0\u05EA \u05D6\u05D9\u05D4\u05D5\u05D9 \u05D4\u05D1\u05E2\u05D9\u05D5\
  \u05EA \u05DC\u05E4\u05E9\u05D5\u05D8 \u05D9\u05D5\u05EA\u05E8. \u05DE\u05EA\u05DB\
  \u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\u05D3\
  \u05D9\u05D1\u05D0\u05D2\u05E8\u05D9\u05DD \u05DC\u05D3\u05D7\u05D9\u05E1\u05EA\
  \ \u05D1\u05D0\u05D2\u05D9\u05DD, \u05DC\u05D4\u05D1\u05E0\u05EA \u05D6\u05E8\u05D9\
  \u05DE\u05EA \u05D4\u05E7\u05D5\u05D3,\u2026"
lastmod: '2024-03-13T22:44:39.558341-06:00'
model: gpt-4-0125-preview
summary: "\u05D3\u05D9\u05D1\u05D0\u05D2\u05E8 \u05D4\u05D5\u05D0 \u05DB\u05DC\u05D9\
  \ \u05E9\u05DE\u05D0\u05E4\u05E9\u05E8 \u05DC\u05DA \u05DC\u05D1\u05D3\u05D5\u05E7\
  \ \u05D5\u05DC\u05E9\u05DC\u05D5\u05D8 \u05D1\u05D1\u05D9\u05E6\u05D5\u05E2 \u05D4\
  \u05EA\u05D5\u05DB\u05E0\u05D9\u05EA, \u05DE\u05D4 \u05E9\u05D4\u05D5\u05E4\u05DA\
  \ \u05D0\u05EA \u05D6\u05D9\u05D4\u05D5\u05D9 \u05D4\u05D1\u05E2\u05D9\u05D5\u05EA\
  \ \u05DC\u05E4\u05E9\u05D5\u05D8 \u05D9\u05D5\u05EA\u05E8. \u05DE\u05EA\u05DB\u05E0\
  \u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\u05D3\u05D9\
  \u05D1\u05D0\u05D2\u05E8\u05D9\u05DD \u05DC\u05D3\u05D7\u05D9\u05E1\u05EA \u05D1\
  \u05D0\u05D2\u05D9\u05DD, \u05DC\u05D4\u05D1\u05E0\u05EA \u05D6\u05E8\u05D9\u05DE\
  \u05EA \u05D4\u05E7\u05D5\u05D3,\u2026"
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05D3\u05D9\u05D1\u05D0\u05D2\u05E8"
---

## איך לעשות:
ל-Lua אין דיבאגר מובנה, אך אפשר להשתמש בחיצוניים, כמו ZeroBrane Studio. הנה טעימה איך עובדים איתו:

```Lua
-- זה סקריפט Lua פשוט עם שגיאה מכוונת
local function add(a, b)
    local result = a + b -- אופס, בואו נדמיין ששכחנו להגדיר את 'b'
    return result
end

print(add(10))
```

כאשר תריץ את זה בדיבאגר, הביצוע יעצר במקום שבו התרחשה הבעיה. תראה משהו כזה:

```
lua: example.lua:3: attempt to perform arithmetic on a nil value (local 'b')
stack traceback:
	example.lua:3: in function 'add'
	example.lua:7: in main chunk
	[C]: in ?
```

ניתן להגדיר נקודות עצירה, לעבור צעד אחר צעד בקוד שלך, ולהציץ בערכי משתנים כדי לאתר את הבאג מבלי לאבד את השפיות.

## צלילה עמוקה
הפשטות של Lua לא משתרעת לדיבאגינג, לצערינו. אך אל דאגה, קהילת Lua מגיעה לעזרה. כלים כמו ZeroBrane Studio, LuaDec, ואחרים מציעים יכולות דיבאגינג. היסטורית, דיבאגרים היו קיימים לא הרבה זמן אחרי שהתכניות הראשונות החלו לסבך, ונתנו למפתחים את האפשרות לתקן את הקוד שלהם מבלי להתעסק בחושך.

עם Lua, לעיתים קרובות אתה מסתמך על דיבאגרים חיצוניים או מתקנם בסביבת הפיתוח שלך. ZeroBrane Studio, למשל, הוא סביבת פיתוח אינטגרטיבית (IDE) שמשלבת בתוכה דיבאגר ל-Lua. הוא מאפשר לך לעבור צעד אחר צעד בקוד, להגדיר נקודות עצירה, ולצפות במשתנים. מבחינת היישום, דיבאגרים בדרך כלל משתמשים ב-hooks כדי להכניס נקודות עצירה וכלי דיבאגינג אחרים.

אלטרנטיבות? כמובן. הודעות `print` הטובות הישנות, המכונות בחיבה "printf debugging", לעיתים יכולות לעשות את העבודה בלי כלים מתוחכמים.

## ראה גם
כדי להמשיך את מסע הדיבאגינג שלך, בדוק:

- ZeroBrane Studio: https://studio.zerobrane.com/
- ויקי משתמשי Lua על דיבאגינג קוד Lua: http://lua-users.org/wiki/DebuggingLuaCode
- התיעוד של ספריית `debug` במדריך ל-Lua: https://www.lua.org/manual/5.4/manual.html#6.10
