---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:37.679155-07:00
description: "YAML, \u05E7\u05D9\u05E6\u05D5\u05E8 \u05E9\u05DC \"YAML Ain't Markup\
  \ Language\" (YAML \u05D0\u05D9\u05E0\u05D5 \u05E9\u05E4\u05EA \u05E1\u05D9\u05DE\
  \u05D5\u05DF), \u05D4\u05D5\u05D0 \u05EA\u05E7\u05DF \u05D4\u05D0\u05E8\u05D9\u05D6\
  \u05EA \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05E7\u05E8\u05D9\u05D0 \u05DC\u05D0\
  \u05D3\u05DD \u05E9\u05E0\u05E2\u05E9\u05D4 \u05D1\u05D5 \u05E9\u05D9\u05DE\u05D5\
  \u05E9 \u05E0\u05E8\u05D7\u05D1 \u05DC\u05E7\u05D1\u05E6\u05D9 \u05E7\u05D5\u05E0\
  \u05E4\u05D9\u05D2\u05D5\u05E8\u05E6\u05D9\u05D4 \u05D5\u05DC\u05D4\u05D7\u05DC\u05E4\
  \u05EA \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD\u2026"
lastmod: '2024-03-13T22:44:39.584550-06:00'
model: gpt-4-0125-preview
summary: "YAML, \u05E7\u05D9\u05E6\u05D5\u05E8 \u05E9\u05DC \"YAML Ain't Markup Language\"\
  \ (YAML \u05D0\u05D9\u05E0\u05D5 \u05E9\u05E4\u05EA \u05E1\u05D9\u05DE\u05D5\u05DF\
  ), \u05D4\u05D5\u05D0 \u05EA\u05E7\u05DF \u05D4\u05D0\u05E8\u05D9\u05D6\u05EA \u05E0\
  \u05EA\u05D5\u05E0\u05D9\u05DD \u05E7\u05E8\u05D9\u05D0 \u05DC\u05D0\u05D3\u05DD\
  \ \u05E9\u05E0\u05E2\u05E9\u05D4 \u05D1\u05D5 \u05E9\u05D9\u05DE\u05D5\u05E9 \u05E0\
  \u05E8\u05D7\u05D1 \u05DC\u05E7\u05D1\u05E6\u05D9 \u05E7\u05D5\u05E0\u05E4\u05D9\
  \u05D2\u05D5\u05E8\u05E6\u05D9\u05D4 \u05D5\u05DC\u05D4\u05D7\u05DC\u05E4\u05EA\
  \ \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D1\u05D9\u05DF \u05E9\u05E4\u05D5\u05EA\
  \ \u05EA\u05DB\u05E0\u05D5\u05EA."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD YAML"
weight: 41
---

## איך ל:
ב-Lua אין תמיכה מובנית ל-YAML, אך ניתן לעבוד עם קבצי YAML באמצעות ספריות צד שלישי כמו `lyaml`. ספרייה זו מאפשרת קידוד ופענוח של נתוני YAML עם Lua. ראשית, תצטרך להתקין את `lyaml` דרך LuaRocks, מנהל החבילות של Lua:

```bash
luarocks install lyaml
```

### פענוח YAML:
נניח שיש לך את תוכן YAML הבא בקובץ בשם `config.yaml`:

```yaml
database:
  host: localhost
  port: 3306
  username: user
  password: pass
```

תוכל לפענח קובץ YAML זה לתוך טבלה של Lua עם הקוד הבא:

```lua
local yaml = require('lyaml')
local file = io.open("config.yaml", "r")
local content = file:read("*all")
file:close()

local data = yaml.load(content)
for k,v in pairs(data.database) do
  print(k .. ": " .. v)
end
```

כאשר תריץ את הסקריפט הזה, הוא אמור להציג:

```output
host: localhost
port: 3306
username: user
password: pass
```

### קידוד YAML:
לשם קידוד טבלאות Lua לפורמט YAML, אתה משתמש בפונקציה `dump` המסופקת על ידי `lyaml`. בהתחשב בכך שאתה רוצה ליצור ייצוג YAML של הטבלה הבאה של Lua:

```lua
local data = {
  website = {
    name = "Example",
    owner = "Jane Doe",
    metadata = {
      creation_date = "2023-01-01",
      tags = {"blog", "personal", "lua"}
    }
  }
}

local yaml = require('lyaml')
local yaml_data = yaml.dump({data})
print(yaml_data)
```

ה-YAML שיוצג יהיה:

```yaml
- website:
    metadata:
      creation_date: '2023-01-01'
      tags: [blog, personal, lua]
    name: Example
    owner: Jane Doe
```

בעקבות התבניות הללו, מתכנתי Lua יכולים לנהל נתוני YAML ביעילות למגוון של יישומים. אופרציות אלו עם YAML הן קריטיות לפיתוח יישומי Lua מגוונים, שמתקשרים בחלקתיות עם חלקים אחרים של מערכת או עם מערכות אחרות ישירות.
