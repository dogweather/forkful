---
date: 2024-01-20 17:53:21.123561-07:00
description: "\u05DB\u05E9\u05D0\u05E0\u05D7\u05E0\u05D5 \u05DE\u05D3\u05E4\u05D9\u05E1\
  \u05D9\u05DD \u05E4\u05DC\u05D8 \u05DC\u05E6\u05D5\u05E8\u05DA \u05D3\u05D9\u05D1\
  \u05D0\u05D2, \u05D0\u05E0\u05D7\u05E0\u05D5 \u05D1\u05E2\u05E6\u05DD \u05D6\u05D5\
  \u05E8\u05E7\u05D9\u05DD \u05D7\u05DC\u05D5\u05DF \u05DC\u05EA\u05D5\u05DA \u05DE\
  \u05D4 \u05E9\u05D4\u05EA\u05D5\u05DB\u05E0\u05D9\u05EA \u05E9\u05DC\u05E0\u05D5\
  \ \u05E2\u05D5\u05E9\u05D4. \u05D6\u05D4 \u05E2\u05D5\u05D6\u05E8 \u05DC\u05E0\u05D5\
  \ \u05DC\u05D0\u05EA\u05E8 \u05D1\u05E2\u05D9\u05D5\u05EA \u05D5\u05DC\u05D1\u05D9\
  \u05DF \u05D0\u05D9\u05DA \u05D4\u05E7\u05D5\u05D3 \u05E9\u05DC\u05E0\u05D5 \u05DE\
  \u05EA\u05E0\u05D4\u05D2 \u05D1\u05D6\u05DE\u05DF \u05D0\u05DE\u05EA."
lastmod: '2024-02-25T18:49:38.443139-07:00'
model: gpt-4-1106-preview
summary: "\u05DB\u05E9\u05D0\u05E0\u05D7\u05E0\u05D5 \u05DE\u05D3\u05E4\u05D9\u05E1\
  \u05D9\u05DD \u05E4\u05DC\u05D8 \u05DC\u05E6\u05D5\u05E8\u05DA \u05D3\u05D9\u05D1\
  \u05D0\u05D2, \u05D0\u05E0\u05D7\u05E0\u05D5 \u05D1\u05E2\u05E6\u05DD \u05D6\u05D5\
  \u05E8\u05E7\u05D9\u05DD \u05D7\u05DC\u05D5\u05DF \u05DC\u05EA\u05D5\u05DA \u05DE\
  \u05D4 \u05E9\u05D4\u05EA\u05D5\u05DB\u05E0\u05D9\u05EA \u05E9\u05DC\u05E0\u05D5\
  \ \u05E2\u05D5\u05E9\u05D4. \u05D6\u05D4 \u05E2\u05D5\u05D6\u05E8 \u05DC\u05E0\u05D5\
  \ \u05DC\u05D0\u05EA\u05E8 \u05D1\u05E2\u05D9\u05D5\u05EA \u05D5\u05DC\u05D1\u05D9\
  \u05DF \u05D0\u05D9\u05DA \u05D4\u05E7\u05D5\u05D3 \u05E9\u05DC\u05E0\u05D5 \u05DE\
  \u05EA\u05E0\u05D4\u05D2 \u05D1\u05D6\u05DE\u05DF \u05D0\u05DE\u05EA."
title: "\u05D4\u05D3\u05E4\u05E1\u05EA \u05E4\u05DC\u05D8 \u05DC\u05E0\u05D9\u05E4\
  \u05D5\u05D9 \u05D1\u05D0\u05D2\u05D9\u05DD"
---

{{< edit_this_page >}}

## What & Why? (מה ולמה?)
כשאנחנו מדפיסים פלט לצורך דיבאג, אנחנו בעצם זורקים חלון לתוך מה שהתוכנית שלנו עושה. זה עוזר לנו לאתר בעיות ולבין איך הקוד שלנו מתנהג בזמן אמת. 

## How to? (איך לעשות?)
```Ruby
# דוגמא: הדפסת הערך של משתנה
x = 'ערך לדוגמא'
puts "ערך המשתנה x הוא: #{x}"

# דוגמא: הדפסת הודעות שגיאה
begin
  # קוד שעלול להעלות שגיאה
rescue => e
  puts "שגיאה נמצאה: #{e.message}"
end

# דוגמא: השימוש ב p להדפסת ערכים עם מידע נוסף
p [1, 2, 3]
```
פלט:
```
ערך המשתנה x הוא: ערך לדוגמא
שגיאה נמצאה: (הודעה ספציפית של שגיאה)
[1, 2, 3]
```

## Deep Dive (צלילה עמוקה)
הדפסת פלט לדיבאג היא כלי זמין מתקופת התכנות הקדומה. היא נעשית בעיקר על ידי פקודות כמו `print` או `puts` ברובי. כאלטרנטיבה, ישנו הקונספט של מערכות לוגינג מתקדמות יותר, שמאפשרות רמות לוגינג שונות. לעיתים, יומנים (logs) נשמרים לקובץ, ולא רק מוצגים על המסך. כאשר אתה משתמש ב `p`, הפונקציה תדפיס את הערך ותשיב אותו, וכך גם תכולתו בפורמט מפורש יותר.

## See Also (ראו גם)
- [המסמך הרשמי של רובי על פקודות דיבאג](https://ruby-doc.org/core-3.1.0/Kernel.html#method-i-puts)
- [תיעוד בנושא מודולים של לוגינג ברובי](https://ruby-doc.org/stdlib-3.1.0/libdoc/logger/rdoc/Logger.html)
