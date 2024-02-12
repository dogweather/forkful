---
title:                "עבודה עם TOML"
aliases:
- /he/lua/working-with-toml.md
date:                  2024-01-26T04:25:05.536911-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/lua/working-with-toml.md"
---

{{< edit_this_page >}}

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
