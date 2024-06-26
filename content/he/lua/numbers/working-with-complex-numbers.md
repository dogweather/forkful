---
date: 2024-01-26 04:44:13.831306-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1-Lua, \u05E0\
  \u05D9\u05EA\u05DF \u05DC\u05D9\u05D9\u05E6\u05D2 \u05DE\u05E1\u05E4\u05E8\u05D9\
  \u05DD \u05DE\u05D3\u05D5\u05DE\u05D9\u05DD \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\
  \u05EA \u05D8\u05D1\u05DC\u05D0\u05D5\u05EA. \u05D4\u05E4\u05E2\u05D5\u05DC\u05D5\
  \u05EA \u05D4\u05D1\u05E1\u05D9\u05E1\u05D9\u05D5\u05EA \u05DB\u05D5\u05DC\u05DC\
  \u05D5\u05EA \u05D4\u05D5\u05E1\u05E4\u05D4, \u05D7\u05D9\u05E1\u05D5\u05E8, \u05DB\
  \u05E4\u05DC \u05D5\u05D7\u05D9\u05DC\u05D5\u05E7 \u05E9\u05DC \u05D8\u05D1\u05DC\
  \u05D0\u05D5\u05EA \u05D0\u05DC\u05D5. \u05D4\u05E0\u05D4 \u05D0\u05D9\u05DA."
lastmod: '2024-03-13T22:44:39.540890-06:00'
model: gpt-4-0125-preview
summary: "\u05D1-Lua, \u05E0\u05D9\u05EA\u05DF \u05DC\u05D9\u05D9\u05E6\u05D2 \u05DE\
  \u05E1\u05E4\u05E8\u05D9\u05DD \u05DE\u05D3\u05D5\u05DE\u05D9\u05DD \u05D1\u05D0\
  \u05DE\u05E6\u05E2\u05D5\u05EA \u05D8\u05D1\u05DC\u05D0\u05D5\u05EA."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD \u05DE\u05E1\u05E4\u05E8\u05D9\
  \u05DD \u05DE\u05E8\u05D5\u05DB\u05D1\u05D9\u05DD"
weight: 14
---

## איך לעשות:
ב-Lua, ניתן לייצג מספרים מדומים באמצעות טבלאות. הפעולות הבסיסיות כוללות הוספה, חיסור, כפל וחילוק של טבלאות אלו. הנה איך:

```lua
-- הגדרת שני מספרים מדומים כטבלאות
local complex_a = { real = 3, imag = 5 }
local complex_b = { real = 2, imag = -4 }

-- פונקציה לחיבור שני מספרים מדומים
local function add_complex(a, b)
  return { real = a.real + b.real, imag = a.imag + b.imag }
end

-- פלט לדוגמה
print(add_complex(complex_a, complex_b))  -- { real = 5, imag = 1 }
```

## צלילה עמוקה
מספרים מדומים קיימים מאז המאה ה-16, ועזרו לפתור משוואות שלא ניתן היה לפתור בעזרת מספרים ממשיים בלבד. ב-Lua עצמו אין סוג מספר מדומה מובנה. עם זאת, זה לא בעיה - ניתן לבנות את התמודדויות שלך עם מספרים מדומים באמצעות טבלאות ופונקציות, כפי שהוצג למעלה. או, אם הצרכים שלך עומקיים יותר, ניתן לשלב ספרייה כמו LuaComplex. זו בחירה טובה מכיוון שהיא בנויה במיוחד עבור Lua והיא מסירה את העבודה הידנית מהדרך. ספריות כאלה לעיתים קרובות מייעלות גם את הפעולות בתוך הקוד, ולכן הן מהירות יותר מלבנות אותן בעצמך.

## ראה גם
לדוגמאות נוספות ופעולות מתקדמות יותר, בדוק את הבאים:

- ספריית LuaComplex: https://github.com/davidm/lua-complex
- ספר "תכנות ב-Lua", ליצירת סוגי נתונים מותאמים אישית: https://www.lua.org/pil/11.1.html
- ויקיפדיה על שימושים של מספרים מדומים בתחומים שונים: https://en.wikipedia.org/wiki/Complex_number#Applications
