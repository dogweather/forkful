---
title:                "עובדים עם CSV"
date:                  2024-02-03T19:21:14.479387-07:00
model:                 gpt-4-0125-preview
simple_title:         "עובדים עם CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/lua/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

עבודה עם קבצי CSV (ערכים מופרדים בפסיק) כוללת ניתוח ויצירת נתוני טקסט המאורגנים לשורות ועמודות, באמצעות שימוש בפסיקים להפרדה בין ערכים בודדים. מתכנתים לעיתים קרובות עוסקים בתהליך זה כדי לקדם החלפת נתונים בין יישומים שונים, בסיסי נתונים, או למטרות עיבוד וניתוח נתונים, בשל התמיכה הנרחבת והפשטות של פורמט הCSV.

## איך לעשות:

בLua, עבודה עם קבצי CSV יכולה להתקיים באמצעות פעולות קלט/פלט קובצים בסיסיות שהשפה מספקת, ללא הצורך בספריות חיצוניות למשימות פשוטות. עם זאת, לטיפול בפעולות מורכבות יותר, כמו טיפול במקרים מיוחדים (לדוגמה, פסיקים בתוך ערכים), יכול להיות רווחי להשתמש בספריות חיצוניות כמו `lua-csv`.

### קריאת קובץ CSV
הנה דוגמה פשוטה לקרוא קובץ CSV שורה אחר שורה, תוך פיצול כל שורה לערכים על פי הפסיק.

```lua
function parseCSVLine(line)
    local result = {}
    local from = 1
    local sep = ","
    local field
    while true do
        local start, finish = string.find(line, sep, from)
        if not start then
            table.insert(result, string.sub(line, from))
            break
        end
        field = string.sub(line, from, start - 1)
        table.insert(result, field)
        from = finish + 1
    end
    return result
end

local file = io.open("example.csv", "r")
for line in file:lines() do
    local values = parseCSVLine(line)
    for i, v in ipairs(values) do
        print(i, v)
    end
end
file:close()
```

**פלט לדוגמה** (עבור `example.csv` עם התוכן "name,age\newlineJohn Doe,30\newlineJane Doe,32"):
```
1	name
2	age
1	John Doe
2	30
1	Jane Doe
2	32
```

### כתיבת קובץ CSV
כדי ליצור קובץ CSV, פשוט מרכיבים מחרוזות עם ערכים המופרדים בפסיקים וכותבים אותם לקובץ שורה אחרי שורה.

```lua
local data = {
    {"name", "age"},
    {"John Doe", "30"},
    {"Jane Doe", "32"}
}

local file = io.open("output.csv", "w")
for _, v in ipairs(data) do
    file:write(table.concat(v, ","), "\n")
end
file:close()
```

פעולה זו תביא ליצירת (או לדריסה) של קובץ `output.csv` עם הנתונים שצוינו.

### שימוש בlua-csv
לטיפול מתקדם יותר בCSV, כולל תמיכה בציטוטים ותווי בריחה, הספריה `lua-csv` היא בחירה עמידה.

ראשית, התקנו אותה באמצעות LuaRocks:
```shell
luarocks install lua-csv
```

לאחר מכן, קריאת קובץ CSV הופכת לפשוטה כמו:

```lua
local csv = require("csv")

-- קריאה מקובץ
for fields in csv.open("example.csv") do
    for i, v in ipairs(fields) do
        print(i, v)
    end
end
```

וכתיבה לCSV עם ציטוט ובריחה כהלכה:

```lua
local file = csv.open("output.csv", {write=true})

local data = {
    {"name", "profession", "location"},
    {"John Doe", "Software Engineer", "New York, NY"},
    {"Jane Doe", "Data Scientist", "\"San Francisco, CA\""}
}

for _, v in ipairs(data) do
    file:write(v)
end
```

גישה זו מטפלת אוטומטית במורכבויות כגון פסיקים וציטוטים בתוך ערכים.
