---
title:                "ניתוח תאריך ממחרוזת"
date:                  2024-01-20T15:37:24.948794-07:00
html_title:           "Arduino: ניתוח תאריך ממחרוזת"
simple_title:         "ניתוח תאריך ממחרוזת"

category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/lua/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
פענוח תאריך ממחרוזת הוא התהליך שבו אתה הופך טקסט לתאריך ניתן לשימוש. תכנתים עושים זאת כדי להכניס, לעבד ולאחסן תאריכים בדרך ממוסדת.

## איך לעשות:
```Lua
-- המרת מחרוזת לתאריך בלואה
function parseDate(dateString)
  local pattern = "(%d+)-(%d+)-(%d+)"
  local year, month, day = dateString:match(pattern)
  return { day = day, month = month, year = year }
end

-- דוגמא לשימוש בפונקציה
local date = parseDate("2023-03-15")
print(date.day .. "/" .. date.month .. "/" .. date.year)  -- הדפסה: 15/03/2023
```

## עיון מעמיק:
בעבר, לואה לא הכילה מודולים טבעיים לניתוח תאריכים, כך שהיה צורך להשתמש בביטויים רגולריים או פונקציות מותאמות אישית. דרכים חלופיות כוללות שימוש בספריות חיצוניות כמו 'date.lua' או הטמעת פונקציה עצמאית כמו למעלה. בתוך הטמעת הפונקציה, `.match` היא דרך יעילה להוציא מרכיבים ממחרוזת באמצעות ביטוי רגולרי.

## ראה גם:
- מסמך המדריך ללואה - http://www.lua.org/manual/5.4/
- ספריית 'date.lua' - http://os.date
- תיעוד ביטויים רגולריים בלואה - https://www.lua.org/pil/20.2.html
