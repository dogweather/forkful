---
title:                "עבודה עם json"
html_title:           "Lua: עבודה עם json"
simple_title:         "עבודה עם json"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/lua/working-with-json.md"
---

{{< edit_this_page >}}

## מה ולמה?
 JSON הוא סוג של תבנית המשמשת לפיענוח ושימוש בנתונים מקודדים. מתכנתים משתמשים ב- JSON כדי להעביר ולשלוח מידע מתוך קוד שפותח לקוד אחר כגון JavaScript.

## איך לעשות:
 בלואה, ניתן להשתמש בספריית "Json" כדי לפענח וליצור נתונים תקניים בפורמט JSON.

```lua
local json = require("json")

-- יצירת עץ JSON
local json_tree = {
  name = "John",
  age = 25,
  interests = {"coding", "reading"}
}

-- המרה לסטרינג JSON
local json_string = json.encode(json_tree)
print(json_string)

-- הפיכת סטרינג JSON לעץ
local decoded_json = json.decode(json_string)
print(decoded_json.name)
```

התוצאה שבתנועה היא:

```
{"name":"John","age":25,"interests":["coding","reading"]}
John
```

## מה עומק:
 JSON נוצר כבולט ב- 1990 על ידי דוויד פורטר כדי להעביר נתונים מרחש צד אחד לצד אחר. ספריות נוספות כגון "dkjson" ניתנות לשימוש בלואה, אך לפני כן אפשרות זו הייתה זמינה רק לשפת גאמר.

## ראה גם:
 למידע נוסף על ספריית "Json" ללואה, בקר באתר של קהילת הלואה הישראלית: https://lualinux.net/forums/viewtopic.php?f=26&t=126077

לפרטים נוספים על תבנית JSON, בקר באתר המקורי: https://json.org/