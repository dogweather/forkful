---
title:                "עבודה עם YAML"
date:                  2024-01-19
simple_title:         "עבודה עם YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/lua/working-with-yaml.md"
---

{{< edit_this_page >}}

## מה ולמה?
YAML הוא פורמט קל לקריאה להגדרת נתונים והגדרות תצורה. תוכנים עובדים עם YAML כדי לשמור מידע בצורה מסודרת ונגישה, שמתאימה גם לאנשים וגם למחשבים.

## איך לעשות:
ב-Lua אין תמיכה יילידית ב-YAML, אז נשתמש בספרייה חיצונית.

התקן את lyaml:
```bash
luarocks install lyaml
```

קוד לעיבוד קובץ YAML:

```Lua
local yaml = require 'lyaml'

-- טען תוכן YAML ממחרוזת
local yaml_data = [[
- name: Tomer
  job: Software Developer
- name: Alex
  job: Data Analyst
]]

-- קרא את ה-YAML והמיר אותו לטבלה של Lua
local people = yaml.load(yaml_data)

-- הדפס את המידע
for _, person in ipairs(people) do
  print(person.name .. " is a " .. person.job)
end
```

פלט לדוגמא:
```
Tomer is a Software Developer
Alex is a Data Analyst
```

## היכנס לעומק:
YAML (YAML Ain't Markup Language) פותח תחילה ב-2001 והוא אלטרנטיבה ל-XML או ל-JSON. YAML מיועד בעיקר למקרים בהם הקריאות לאדם חשובה מאוד, כמו קבצי תצורה. יש כמה ביבליות Lua לעבודה עם YAML, אבל 'lyaml' היא אחת מהנפוצות.

## ראה גם:
- המסמך הרשמי של YAML - https://yaml.org/spec/1.2/spec.html
- lyaml ב-GitHub - https://github.com/gvvaughan/lyaml
- עמוד הספרייה ב-LuaRocks - https://luarocks.org/modules/gvvaughan/lyaml
