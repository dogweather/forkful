---
title:                "עבודה עם קבצי CSV"
date:                  2024-01-19
html_title:           "Arduino: עבודה עם קבצי CSV"
simple_title:         "עבודה עם קבצי CSV"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/lua/working-with-csv.md"
---

{{< edit_this_page >}}

## מה ולמה?

עבודה עם קבצי CSV משמעותה לקרוא ולכתוב נתונים בפורמט טקסט פשוט שבו הערכים מופרדים בפסיקים. תכניתנים עושים זאת כי זה מאפשר החלפת נתונים עם תוכנות אחרות בצורה קלה וגמישה.

## איך עושים את זה?

קוד לקריאת CSV:
```Lua
function parse_csv(input_string)
  local results = {}
  for line in input_string:gmatch("[^\r\n]+") do
    local row = {}
    for value in line:gmatch("[^,]+") do
      table.insert(row, value)
    end
    table.insert(results, row)
  end
  return results
end

local csv_data = "שם,גיל\nדני,30\nרחל,25"
local parsed_data = parse_csv(csv_data)
for i, row in ipairs(parsed_data) do
  print(row[1], row[2])
end
```

פלט דוגמא:
```
דני 30
רחל 25
```

קוד לכתיבת CSV:
```Lua
function to_csv(data)
  local csv_string = ""
  for _, row in ipairs(data) do
    csv_string = csv_string .. table.concat(row, ",") .. "\n"
  end
  return csv_string
end

local people = {{"נעמי", "22"}, {"אלון", "33"}}
local csv_output = to_csv(people)
print(csv_output)
```

פלט דוגמא:
```
נעמי,22
אלון,33
```

## עומק הנושא

פורמט CSV התפתח בשנות ה-70 כדרך פשוטה לייצוא וייבוא נתונים בין תוכנות שונות. אלטרנטיבות פחות פשוטות לפורמט כוללות JSON או XML, שניהם מספקים עושר ייצוג נתונים גדול יותר. בנוגע לCSV, פרטי היישום יכולים להיות מורכבים מאחר והפורמט אינו ממש ממותג וישנן הרחבות כמו תמיכה בטקסט מוקף בתיבות או פסיקים בתוך ערכים.

## ראו גם

- Lua 5.4 רשמי [המדריך העצמי](https://www.lua.org/manual/5.4/)
- [RFC 4180](https://tools.ietf.org/html/rfc4180) - תקן שמגדיר את הפורמט של הCSV
- ספריית Lua [Penlight](https://github.com/lunarmodules/Penlight), שכוללת פונקציות לעבודה עם CSV
- כלים מקוונים להמרת קבצי CSV לפורמטים אחרים, כמו [CSV to JSON](https://csvjson.com/csv2json)
