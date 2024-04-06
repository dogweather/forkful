---
date: 2024-01-20 17:34:12.500301-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05DC\u05D4\u05E9\
  \u05D5\u05D5\u05EA \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD \u05D4\u05D9\u05D0\
  \ \u05E4\u05E2\u05D5\u05DC\u05D4 \u05E9\u05DE\u05D5\u05E8\u05DB\u05D1\u05EA \u05DE\
  \u05D4\u05E9\u05D5\u05D5\u05D0\u05EA \u05D4\u05E9\u05E0\u05D9\u05DD, \u05D4\u05D7\
  \u05D5\u05D3\u05E9\u05D9\u05DD \u05D5\u05D4\u05D9\u05DE\u05D9\u05DD \u05D1\u05D0\
  \u05D5\u05E4\u05DF \u05E1\u05D3\u05E8\u05EA\u05D9. \u05D7\u05E9\u05D5\u05D1 \u05DC\
  \u05D6\u05DB\u05D5\u05E8, \u05E2\u05D3 Ruby 1.9, \u05D4\u05D9\u05D9\u05EA\u05D4\
  \ \u05E6\u05D5\u05E8\u05DA \u05DC\u05D3\u05D0\u05D5\u05D2 \u05DC\u05D9\u05E6\u05D9\
  \u05E8\u05EA \u05D0\u05D5\u05D1\u05D9\u05D9\u05E7\u05D8\u05D9 `Time` \u05D0\u05D5\
  \u2026"
lastmod: '2024-04-05T22:50:54.240606-06:00'
model: gpt-4-1106-preview
summary: "\u05DC\u05D4\u05E9\u05D5\u05D5\u05EA \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\
  \u05DD \u05D4\u05D9\u05D0 \u05E4\u05E2\u05D5\u05DC\u05D4 \u05E9\u05DE\u05D5\u05E8\
  \u05DB\u05D1\u05EA \u05DE\u05D4\u05E9\u05D5\u05D5\u05D0\u05EA \u05D4\u05E9\u05E0\
  \u05D9\u05DD, \u05D4\u05D7\u05D5\u05D3\u05E9\u05D9\u05DD \u05D5\u05D4\u05D9\u05DE\
  \u05D9\u05DD \u05D1\u05D0\u05D5\u05E4\u05DF \u05E1\u05D3\u05E8\u05EA\u05D9."
title: "\u05D4\u05E9\u05D5\u05D5\u05D0\u05EA \u05E9\u05EA\u05D9 \u05EA\u05D0\u05E8\
  \u05D9\u05DB\u05D9\u05DD"
weight: 27
---

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
