---
date: 2024-01-20 17:34:09.902121-07:00
description: "\u05D4\u05E9\u05D5\u05D5\u05D0\u05EA \u05E9\u05EA\u05D9 \u05EA\u05D0\
  \u05E8\u05D9\u05DB\u05D9\u05DD \u05DE\u05E7\u05D1\u05D9\u05DC\u05D4 \u05DC\u05D4\
  \u05E9\u05D5\u05D5\u05D0\u05EA \u05E9\u05E0\u05D9 \u05E4\u05E8\u05E7\u05D9 \u05D6\
  \u05DE\u05DF. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\u05D5\u05D5\
  \u05D9\u05DD \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD \u05DB\u05D3\u05D9 \u05DC\
  \u05D7\u05E9\u05D1 \u05E4\u05E2\u05E8\u05D9\u05DD, \u05DC\u05D1\u05D3\u05D5\u05E7\
  \ \u05D7\u05D5\u05E7\u05D9\u05D5\u05EA \u05D5\u05DC\u05E1\u05D3\u05E8 \u05D0\u05D9\
  \u05E8\u05D5\u05E2\u05D9\u05DD \u05DC\u05E4\u05D9 \u05E6\u05D9\u05E8 \u05D6\u05DE\
  \u05DF."
lastmod: '2024-02-25T18:49:37.810728-07:00'
model: gpt-4-1106-preview
summary: "\u05D4\u05E9\u05D5\u05D5\u05D0\u05EA \u05E9\u05EA\u05D9 \u05EA\u05D0\u05E8\
  \u05D9\u05DB\u05D9\u05DD \u05DE\u05E7\u05D1\u05D9\u05DC\u05D4 \u05DC\u05D4\u05E9\
  \u05D5\u05D5\u05D0\u05EA \u05E9\u05E0\u05D9 \u05E4\u05E8\u05E7\u05D9 \u05D6\u05DE\
  \u05DF. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\u05D5\u05D5\u05D9\
  \u05DD \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD \u05DB\u05D3\u05D9 \u05DC\u05D7\
  \u05E9\u05D1 \u05E4\u05E2\u05E8\u05D9\u05DD, \u05DC\u05D1\u05D3\u05D5\u05E7 \u05D7\
  \u05D5\u05E7\u05D9\u05D5\u05EA \u05D5\u05DC\u05E1\u05D3\u05E8 \u05D0\u05D9\u05E8\
  \u05D5\u05E2\u05D9\u05DD \u05DC\u05E4\u05D9 \u05E6\u05D9\u05E8 \u05D6\u05DE\u05DF\
  ."
title: "\u05D4\u05E9\u05D5\u05D5\u05D0\u05EA \u05E9\u05EA\u05D9 \u05EA\u05D0\u05E8\
  \u05D9\u05DB\u05D9\u05DD"
---

{{< edit_this_page >}}

## מה ולמה?
השוואת שתי תאריכים מקבילה להשוואת שני פרקי זמן. מתכנתים משווים תאריכים כדי לחשב פערים, לבדוק חוקיות ולסדר אירועים לפי ציר זמן.

## איך לעשות:
ב-Lua, אתה לא משווה ישירות תאריכים. אתה הופך אותם לשניות (timestamp) ומשוווה את הערכים:
```lua
os = require("os")

-- המרת תאריך לתג הזמן
function convertToTimestamp(year, month, day)
  return os.time{year=year, month=month, day=day}
end

-- נתונים להשוואה
date1 = convertToTimestamp(2023, 4, 1)
date2 = convertToTimestamp(2023, 4, 15)

-- השוואת התאריכים
if date1 > date2 then
  print("תאריך 1 מאוחר יותר מתאריך 2")
elseif date1 < date2 then
  print("תאריך 1 מוקדם יותר מתאריך 2")
else
  print("התאריכים זהים")
end
```
פלט לדוגמה:
```
תאריך 1 מוקדם יותר מתאריך 2
```

## צלילה עמוקה
בעבר, מתכנתי Lua היו מסתמכים על ליבות חיצוניות לניהול תאריכים מורכבים יותר. כיום, מודול 'os' מספק כלים טובים לעבודה עם זמן ותאריכים. עם זאת, למודול זה יש הגבלות, ולעתים תצטרך לשלב ספריות שלישיות לתמיכה מלאה באזורי זמן ופורמטים מורכבים יותר.

מתכנתים שנעזרים במודולי זמן ותאריך צריכים לזכור לעדכן אותם כאשר מתכנסים שינויים בכללי אזורי זמן, כמו התאמות שעון קיץ.

## קישורים נוספים
הנה כמה משאבים שיכולים להיות מועילים להרחבת הידע שלך:
- [תיעוד Lua אודות os.time](https://www.lua.org/manual/5.4/manual.html#pdf-os.time)
