---
title:                "עבודה עם JSON"
date:                  2024-01-19
simple_title:         "עבודה עם JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/lua/working-with-json.md"
---

{{< edit_this_page >}}

## מה ולמה?
JSON הוא פורמט חליפין נתונים, פשוט לקריאה וכתיבה על ידי בני אדם, ולעיבוד על ידי מכונות. תכניתנים משתמשים ב-JSON בגלל התאימות הרחבה והיכולת להמיר בקלות מבני נתונים למחרוזת וחזרה.

## איך לעשות:
```Lua
-- טעינת ספריית cjson
local cjson = require "cjson"

-- יצירת אובייקט JSON מתוך מילון
local my_table = { name = "Yossi", age = 30, is_programmer = true }
local json_string = cjson.encode(my_table)
print(json_string)
-- פלט: {"name":"Yossi","age":30,"is_programmer":true}

-- המרת מחרוזת JSON למילון
local decoded_table = cjson.decode(json_string)
print(decoded_table.name, decoded_table.age)
-- פלט: Yossi 30
```

## צלילה לעומק
השימוש ב-JSON כמערכת נתונים התחיל בשנים המוקדמות של המילניום עם הטמעה ב-JavaScript. הלטרנטיבות כוללות XML ו-YAML, אך JSON נשאר פופולרי עקב פשטותו. ב-Lua, המרת JSON מתבצעת באמצעות ספריות חיצוניות כמו cjson או dkjson, שמאפשרות את הטמעת הפונקציות הדרושות.

## ראה גם
- מדריך לספריית cjson: https://www.kyne.com.au/~mark/software/lua-cjson-manual.html
- מדריך לספריית dkjson: http://dkolf.de/src/dkjson-lua.fsl/home
- השוואת פורמטים לחילופי נתונים: https://www.json.org/json-en.html
