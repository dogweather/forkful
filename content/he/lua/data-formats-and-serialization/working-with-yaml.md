---
title:                "עבודה עם YAML"
aliases: - /he/lua/working-with-yaml.md
date:                  2024-02-03T19:26:37.679155-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/lua/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

YAML, קיצור של "YAML Ain't Markup Language" (YAML אינו שפת סימון), הוא תקן האריזת נתונים קריא לאדם שנעשה בו שימוש נרחב לקבצי קונפיגורציה ולהחלפת נתונים בין שפות תכנות. מתכנתים מנצלים את YAML בזכות הפשטות והנגישות שלו, הופך אותו לבחירה מועדפת להגדרות, קונפיגורציות שונות של יישומים, או תוכן שאמור להיות נערך על ידי אנשים שאינם מתכנתים.

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
