---
title:                "השוואה בין שני תאריכים"
html_title:           "Arduino: השוואה בין שני תאריכים"
simple_title:         "השוואה בין שני תאריכים"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/lua/comparing-two-dates.md"
---

{{< edit_this_page >}}

## מה ולמה?
השוואה בין שני תאריכים בתכנות היא הבדלה בין הצגות של שני מופעים של תאריך-זמן. תוכנתים משתמשים בה לגבש מסקנות על המרחק בזמן בין שני אירועים, תהליך שהכרחי במגוון רחב של תוכנות.

## איך לעשות את זה:
הנה שני קטעי קוד שמדגימים את אופן השוואת שני תאריכים ב-Lua:

```Lua
os.setlocale(os.setlocale(''), 'time') -- שנה את הלוקטורה בהתאם לגרמנית
local t1 = os.time{year=2022, month=1, day=1}
local t2 = os.time{year=2022, month=1, day=2}
local diff = os.difftime(t2, t1)
print(diff)
```

פלט:
```Lua
86400
```

שימו לב שהפלט הוא בשניות. דוגמה נוספת:

```Lua
os.setlocale(os.setlocale(''), 'time')
local format = '%d-%m-%Y %H:%M:%S'
local dt1 = os.date('*t', os.time{year=2022, month=1, day=1, hour=0})
local dt2 = os.date('*t', os.time{year=2022, month=1, day=2, hour=12})
print(os.date(format, os.time(dt1))) -- הדפס תאריך 1
print(os.date(format, os.time(dt2))) -- הדפס תאריך 2
local diff = os.difftime(os.time(dt2), os.time(dt1))
print(diff)
```

פלט:
```Lua
01-01-2022 00:00:00
02-01-2022 12:00:00
129600
```

## צונה עמוקה
הבדלה בין תאריכים היא בעצם השוואה של חותמות זמן, ו-Lua אינה כוללת פונקציות מובנות להשוואות של תאריכים, אך מספקת את פונקציה 'os.difftime' ששמה את שני התאריכים כארגומנטים ומחזירה את ההבדל ביניהם בשניות. כאשר יש דרישה ספציפית יותר, יש לשלב ספריות נוספות.

גם אם os.difftime מוגדרת עבור השווי של השניות ביניהם, אתה יכול להמיר את השניות לשעות, דקות, שנים ועוד באמצעות פונקציות עזר.

## ראה גם:
- [Lua os.difftime](http://www.lua.org/manual/5.3/manual.html#pdf-os.difftime)
- [Lua os.time](http://www.lua.org/manual/5.3/manual.html#pdf-os.time)
- [שוואה של תאריכים ב-Lua](https://stackoverflow.com/questions/19864482/comparing-two-dates-in-lua)