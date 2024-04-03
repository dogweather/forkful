---
date: 2024-01-26 04:25:05.536911-07:00
description: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD TOML \u05DB\u05D5\u05DC\u05DC\
  \u05EA \u05E0\u05D9\u05EA\u05D5\u05D7 \u05D5\u05D9\u05D9\u05E6\u05D5\u05E8 \u05E0\
  \u05EA\u05D5\u05E0\u05D9 TOML (Tom's Obvious, Minimal Language - \u05E9\u05E4\u05EA\
  \ \u05EA\u05DB\u05E0\u05D5\u05EA \u05DE\u05D5\u05D1\u05E0\u05EA \u05D5\u05DE\u05D9\
  \u05E0\u05D9\u05DE\u05DC\u05D9\u05E1\u05D8\u05D9\u05EA \u05E9\u05DC \u05D8\u05D5\
  \u05DD) \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA Lua. \u05EA\u05DB\u05E0\u05EA\
  \u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1-TOML\u2026"
lastmod: '2024-03-13T22:44:39.589478-06:00'
model: gpt-4-0125-preview
summary: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD TOML \u05DB\u05D5\u05DC\u05DC\
  \u05EA \u05E0\u05D9\u05EA\u05D5\u05D7 \u05D5\u05D9\u05D9\u05E6\u05D5\u05E8 \u05E0\
  \u05EA\u05D5\u05E0\u05D9 TOML (Tom's Obvious, Minimal Language - \u05E9\u05E4\u05EA\
  \ \u05EA\u05DB\u05E0\u05D5\u05EA \u05DE\u05D5\u05D1\u05E0\u05EA \u05D5\u05DE\u05D9\
  \u05E0\u05D9\u05DE\u05DC\u05D9\u05E1\u05D8\u05D9\u05EA \u05E9\u05DC \u05D8\u05D5\
  \u05DD) \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA Lua."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD TOML"
weight: 39
---

## מה ולמה?
עבודה עם TOML כוללת ניתוח וייצור נתוני TOML (Tom's Obvious, Minimal Language - שפת תכנות מובנת ומינימליסטית של טום) באמצעות Lua. תכנתים משתמשים ב-TOML עבור קבצי תצורה בשל קריאותה ותחבירה הפשוט שהופך בקלות למבנה נתונים.

## איך לעשות:
ראשית, וודאו שסביבת ה-Lua שלכם כוללת מנתח TOML. בדוגמה זו נשתמש ב-`lua-toml`.

```Lua
local toml = require("toml")

-- ניתוח מחרוזת TOML
local toml_data = [[
title = "דוגמת TOML"

[owner]
name = "טום פרסטון-וורנר"
dob = 1979-05-27T07:32:00Z
]]

local data = toml.parse(toml_data)
print(data.title) -- "דוגמת TOML"

-- ייצור מחרוזת TOML
local table_data = {
  title = "דוגמת TOML",
  owner = {
    name = "טום פרסטון-וורנר",
    dob = os.time({year=1979, month=5, day=27, hour=7, min=32})
  }
}

local toml_string = toml.encode(table_data)
print(toml_string)
```

פלט לדוגמה:
```
דוגמת TOML
```

## ניתוח מעמיק
TOML נוצר על ידי טום פרסטון-וורנר בשנת 2013 כחלופה לשפות סידור נתונים אחרות כמו XML ו-YAML, ומציע פורמט יותר פשוט לייצוג נתוני תצורה. בזמן ש-JJSON התפשט בכל מקום, התחביר שלו יכול להיות מסורבל עבור קבצי תצורה. TOML מזהיר עם תחביר ברור יותר לאנושי, הדומה לקבצי .ini אך עם יכולת לקינון וסוגי נתונים.

חלופות ל-TOML כוללות JSON, YAML, ו-XML. עם זאת, TOML מתוכנן במיוחד עבור תצורה והוא באופן טיעוני פשוט יותר מ-YAML, יותר קריא מ-JSON למטרות תצורה, ופחות ארוך מ-XML.

יישום של טיפול ב-TOML ב-Lua בדרך כלל דורש ספרייה חיצונית. הביצועים והתכונות יכולים להשתנות, החל מניתוח בסיסי ועד לתמיכה מלאה בסידור. כאשר מתמודדים עם קבצי תצורה גדולים או פעולות קריאה/כתיבה תכופות, שקלו את ביצועי הספרייה והתאמתה לגרסת TOML העדכנית ביותר.

## ראו גם
- מפרט TOML: https://toml.io/en/
- ספריית `lua-toml`: https://github.com/jonstoler/lua-toml
- השוואה של פורמטים לסידור נתונים: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
