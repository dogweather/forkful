---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:49.205221-07:00
description: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD JSON \u05D1-Lua \u05DB\u05D5\
  \u05DC\u05DC\u05EA \u05E0\u05D9\u05EA\u05D5\u05D7 \u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05D5\u05EA \u05D1\u05E4\u05D5\u05E8\u05DE\u05D8 JSON \u05DC\u05D8\u05D1\u05DC\u05D0\
  \u05D5\u05EA Lua \u05D5\u05D1\u05D7\u05D6\u05E8\u05D4, \u05DE\u05D4 \u05E9\u05DE\
  \u05D0\u05E4\u05E9\u05E8 \u05D4\u05D7\u05DC\u05E4\u05EA \u05E0\u05EA\u05D5\u05E0\
  \u05D9\u05DD \u05E7\u05DC\u05D4 \u05D1\u05D9\u05DF \u05D0\u05E4\u05DC\u05D9\u05E7\
  \u05E6\u05D9\u05D5\u05EA Lua \u05DC\u05E9\u05D9\u05E8\u05D5\u05EA\u05D9 \u05D5\u05D5\
  \u05D1 \u05D0\u05D5 \u05DC-APIs \u05D7\u05D9\u05E6\u05D5\u05E0\u05D9\u05D9\u05DD\
  .\u2026"
lastmod: '2024-03-13T22:44:39.586350-06:00'
model: gpt-4-0125-preview
summary: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD JSON \u05D1-Lua \u05DB\u05D5\
  \u05DC\u05DC\u05EA \u05E0\u05D9\u05EA\u05D5\u05D7 \u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05D5\u05EA \u05D1\u05E4\u05D5\u05E8\u05DE\u05D8 JSON \u05DC\u05D8\u05D1\u05DC\u05D0\
  \u05D5\u05EA Lua \u05D5\u05D1\u05D7\u05D6\u05E8\u05D4, \u05DE\u05D4 \u05E9\u05DE\
  \u05D0\u05E4\u05E9\u05E8 \u05D4\u05D7\u05DC\u05E4\u05EA \u05E0\u05EA\u05D5\u05E0\
  \u05D9\u05DD \u05E7\u05DC\u05D4 \u05D1\u05D9\u05DF \u05D0\u05E4\u05DC\u05D9\u05E7\
  \u05E6\u05D9\u05D5\u05EA Lua \u05DC\u05E9\u05D9\u05E8\u05D5\u05EA\u05D9 \u05D5\u05D5\
  \u05D1 \u05D0\u05D5 \u05DC-APIs \u05D7\u05D9\u05E6\u05D5\u05E0\u05D9\u05D9\u05DD\
  .\u2026"
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD JSON"
weight: 38
---

## מה ולמה?
עבודה עם JSON ב-Lua כוללת ניתוח מחרוזות בפורמט JSON לטבלאות Lua ובחזרה, מה שמאפשר החלפת נתונים קלה בין אפליקציות Lua לשירותי ווב או ל-APIs חיצוניים. מתכנתים עושים זאת כדי לנצל את הפורמט הקל והקל לניתוח של JSON לאחסון נתונים יעיל, תצורה, או תקשורת API.

## איך לעשות:
Lua אינה כוללת ספריה מובנית לעיבוד JSON. לכן, אחת הספריות הצד שלישי הפופולריות היא `dkjson`, שאתה יכול להשתמש בה בקלות לקידוד ולפענוח של JSON. תחילה, וודא שהתקנת את `dkjson`, למשל דרך LuaRocks (`luarocks install dkjson`), ולאחר מכן עקוב אחרי הדוגמאות למטה.

### פענוח JSON לטבלת Lua
```lua
local dkjson = require "dkjson"

local jsonString = '{"name": "Lua Programmer", "age": 30, "languages": ["Lua", "JavaScript"]}'
local luaTable, pos, err = dkjson.decode(jsonString, 1, nil)
if err then
  print ("Error:", err)
else
  print("Name:", luaTable.name) -- פלט: Name: Lua Programmer
  print("Age:", luaTable.age) -- פלט: Age: 30
  print("Languages:", table.concat(luaTable.languages, ", ")) -- פלט: Languages: Lua, JavaScript
end
```

### קידוד טבלת Lua ל-JSON
```lua
local dkjson = require "dkjson"

local luaTable = {
  name = "Lua Programmer",
  age = 30,
  languages = { "Lua", "JavaScript" }
}

local jsonString = dkjson.encode(luaTable, { indent = true })
print(jsonString)
```

דוגמה לפלט של קידוד:
```json
{
  "age": 30,
  "languages": [
    "Lua",
    "JavaScript"
  ],
  "name": "Lua Programmer"
}
```

דוגמאות פשוטות אלו מדגימות איך לעבוד עם JSON ב-Lua, הופכות את התהליך לקל לשילוב של אפליקציות Lua עם טכנולוגיות ווב שונות ו-APIs חיצוניים. זכור, בעוד ש`dkjson` משמש בדוגמאות אלו, ספריות אחרות כמו `cjson` ו`RapidJSON` יכולות גם להיות חלופות מתאימות בהתאם לצרכים של הפרויקט שלך.
