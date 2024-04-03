---
date: 2024-01-20 17:41:28.947094-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1\u05DC\u05D5\
  \u05D0\u05D4, \u05D0\u05D9\u05DF \u05DE\u05D5\u05D3\u05D5\u05DC \u05DE\u05D5\u05D1\
  \u05E0\u05D4 \u05DC\u05D9\u05E6\u05D9\u05E8\u05EA \u05E7\u05D1\u05E6\u05D9\u05DD\
  \ \u05D6\u05DE\u05E0\u05D9\u05D9\u05DD, \u05D0\u05D6 \u05E0\u05E6\u05D8\u05E8\u05DA\
  \ \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1`os.tmpname()` \u05DC\u05D9\u05E6\u05D9\
  \u05E8\u05EA \u05E9\u05DD \u05E7\u05D5\u05D1\u05E5 \u05D6\u05DE\u05E0\u05D9 \u05D5\
  \u05D1`io.open()` \u05DC\u05E4\u05EA\u05D9\u05D7\u05EA \u05D4\u05E7\u05D5\u05D1\u05E5\
  ."
lastmod: '2024-03-13T22:44:39.583051-06:00'
model: gpt-4-1106-preview
summary: "\u05D1\u05DC\u05D5\u05D0\u05D4, \u05D0\u05D9\u05DF \u05DE\u05D5\u05D3\u05D5\
  \u05DC \u05DE\u05D5\u05D1\u05E0\u05D4 \u05DC\u05D9\u05E6\u05D9\u05E8\u05EA \u05E7\
  \u05D1\u05E6\u05D9\u05DD \u05D6\u05DE\u05E0\u05D9\u05D9\u05DD, \u05D0\u05D6 \u05E0\
  \u05E6\u05D8\u05E8\u05DA \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1`os.tmpname()`\
  \ \u05DC\u05D9\u05E6\u05D9\u05E8\u05EA \u05E9\u05DD \u05E7\u05D5\u05D1\u05E5 \u05D6\
  \u05DE\u05E0\u05D9 \u05D5\u05D1`io.open()` \u05DC\u05E4\u05EA\u05D9\u05D7\u05EA\
  \ \u05D4\u05E7\u05D5\u05D1\u05E5."
title: "\u05D9\u05E6\u05D9\u05E8\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D6\u05DE\u05E0\
  \u05D9"
weight: 21
---

## איך לעשות:
בלואה, אין מודול מובנה ליצירת קבצים זמניים, אז נצטרך להשתמש ב`os.tmpname()` ליצירת שם קובץ זמני וב`io.open()` לפתיחת הקובץ.

```Lua
local temp_filename = os.tmpname()
local temp_file, err = io.open(temp_filename, "w")

if temp_file then
    temp_file:write("זהו תוכן זמני בקובץ.")
    temp_file:close()
else
    print("לא ניתן ליצור קובץ זמני:", err)
end

-- זכור למחוק את הקובץ כאשר אתה סיים להשתמש בו!
os.remove(temp_filename)
```

שימו לב שהפונקציה `os.tmpname` יוצרת רק שם קובץ, אבל לא את הקובץ עצמו - לזה אנחנו צריכים את `io.open`.

## עיון נוסף:
יוצרים קבצים זמניים כדי להבטיח שניתן להשתמש בנתונים בצורה בטוחה ללא חשש להשפעות על קבצים פרמננטיים. בעבר, יצירת קבצים זמניים היתה פעולה מורכבת יותר, דורשת דאגה לניקוי אחר כך. גם היום חשוב לזכור למחוק את הקובץ כדי למנוע בעיות וזבל דיסק. במערכות אחרות יש מודולים שמקלים על זה, כמו `tempfile` בפייתון. בלואה, יהיה עליך לטפל בזה בעצמך.

## ראה גם:
- [Lua 5.4 Reference Manual on I/O](https://www.lua.org/manual/5.4/manual.html#6.8)
- [Lua 5.4 Reference Manual on Operating System Facilities](https://www.lua.org/manual/5.4/manual.html#6.9)
- תיעוד על `io` בלואה: https://www.lua.org/pil/21.html
- תיעוד על `os` בלואה: https://www.lua.org/pil/22.1.html
