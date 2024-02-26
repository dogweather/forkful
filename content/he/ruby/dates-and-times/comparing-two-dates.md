---
date: 2024-01-20 17:34:12.500301-07:00
description: "\u05DC\u05D4\u05E9\u05D5\u05D5\u05EA \u05E9\u05EA\u05D9 \u05EA\u05D0\
  \u05E8\u05D9\u05DB\u05D9\u05DD \u05D6\u05D4 \u05DC\u05D1\u05D3\u05D5\u05E7 \u05D0\
  \u05D9\u05D6\u05D4 \u05DE\u05D4\u05DD \u05E7\u05E8\u05D4 \u05E7\u05D5\u05D3\u05DD\
  , \u05D0\u05D5 \u05D0\u05DD \u05D4\u05DD \u05E7\u05E8\u05D5 \u05D1\u05D0\u05D5\u05EA\
  \u05D5 \u05D6\u05DE\u05DF. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\
  \u05D9\u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05DE\u05D9\u05D9\
  \u05DF \u05D0\u05D9\u05E8\u05D5\u05E2\u05D9\u05DD, \u05DC\u05E2\u05E7\u05D5\u05D1\
  \ \u05D0\u05D7\u05E8\u05D9 \u05D6\u05DE\u05E0\u05D9\u05DD \u05D5\u05EA\u05D0\u05E8\
  \u05D9\u05DB\u05D9 \u05D4\u05D2\u05E9\u05D4, \u05D5\u05DC\u05E0\u05D4\u05DC \u05DC\
  \u05D5\u05D7\u05D5\u05EA\u2026"
lastmod: '2024-02-25T18:49:38.459775-07:00'
model: gpt-4-1106-preview
summary: "\u05DC\u05D4\u05E9\u05D5\u05D5\u05EA \u05E9\u05EA\u05D9 \u05EA\u05D0\u05E8\
  \u05D9\u05DB\u05D9\u05DD \u05D6\u05D4 \u05DC\u05D1\u05D3\u05D5\u05E7 \u05D0\u05D9\
  \u05D6\u05D4 \u05DE\u05D4\u05DD \u05E7\u05E8\u05D4 \u05E7\u05D5\u05D3\u05DD, \u05D0\
  \u05D5 \u05D0\u05DD \u05D4\u05DD \u05E7\u05E8\u05D5 \u05D1\u05D0\u05D5\u05EA\u05D5\
  \ \u05D6\u05DE\u05DF. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\
  \u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05DE\u05D9\u05D9\u05DF\
  \ \u05D0\u05D9\u05E8\u05D5\u05E2\u05D9\u05DD, \u05DC\u05E2\u05E7\u05D5\u05D1 \u05D0\
  \u05D7\u05E8\u05D9 \u05D6\u05DE\u05E0\u05D9\u05DD \u05D5\u05EA\u05D0\u05E8\u05D9\
  \u05DB\u05D9 \u05D4\u05D2\u05E9\u05D4, \u05D5\u05DC\u05E0\u05D4\u05DC \u05DC\u05D5\
  \u05D7\u05D5\u05EA\u2026"
title: "\u05D4\u05E9\u05D5\u05D5\u05D0\u05EA \u05E9\u05EA\u05D9 \u05EA\u05D0\u05E8\
  \u05D9\u05DB\u05D9\u05DD"
---

{{< edit_this_page >}}

## מה ולמה?
להשוות שתי תאריכים זה לבדוק איזה מהם קרה קודם, או אם הם קרו באותו זמן. תכנתים עושים את זה כדי למיין אירועים, לעקוב אחרי זמנים ותאריכי הגשה, ולנהל לוחות זמנים.

## איך לעשות:
```Ruby
require 'date'

# יצירת שני אובייקטים מסוג Date
date1 = Date.new(2023, 3, 14)
date2 = Date.new(2023, 5, 28)

# השוואה לפי מי מוקדם יותר
if date1 < date2
  puts "date1 is earlier"
elsif date1 > date2
  puts "date2 is earlier"
else
  puts "The dates are the same"
end
```
פלט דוגמא:
```
date1 is earlier
```

## צלילה לעומק:
להשוות תאריכים היא פעולה שמורכבת מהשוואת השנים, החודשים והימים באופן סדרתי. חשוב לזכור, עד Ruby 1.9, הייתה צורך לדאוג ליצירת אובייקטי `Time` או `DateTime`, אבל כיום מומלץ להשתמש במחלקת `Date` לפשטות וקונסיסטנטיות. חלופות כוללות ספריות חיצוניות כמו 'ActiveSupport' מ-Rails, המאפשרת השוואת תאריכים באמצעות הוספת ימים או חודשים ישירות לאובייקטי `Date` ו`Time`.

## ראה גם:
- תיעוד Ruby על מחלקת [Date](https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/Date.html)
- גמר הסבר על `ActiveSupport` [Time Extensions](https://api.rubyonrails.org/classes/ActiveSupport/TimeWithZone.html)
- כיצד להשתמש ב-[DateTime](https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/DateTime.html) להשוואות מתקדמות יותר
