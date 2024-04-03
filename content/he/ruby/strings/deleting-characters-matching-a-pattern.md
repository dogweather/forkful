---
date: 2024-01-20 17:43:02.186774-07:00
description: "\u05DE\u05D7\u05D9\u05E7\u05EA \u05EA\u05D5\u05D5\u05D9\u05DD \u05D4\
  \u05EA\u05D5\u05D0\u05DE\u05D9\u05DD \u05DC\u05EA\u05D1\u05E0\u05D9\u05EA \u05D1\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D4\u05D9\u05D0 \u05E4\u05E2\u05D5\u05DC\
  \u05D4 \u05E9\u05DE\u05D0\u05E4\u05E9\u05E8\u05EA \u05DC\u05E0\u05E7\u05D5\u05EA\
  \ \u05DE\u05D9\u05D3\u05E2 \u05DE\u05DC\u05DB\u05DC\u05D5\u05DA \u05D0\u05D5 \u05E4\
  \u05D5\u05E8\u05DE\u05D8 \u05DE\u05D9\u05D5\u05EA\u05E8, \u05DB\u05DE\u05D5 \u05EA\
  \u05D5\u05D5\u05D9 \u05D8\u05E7\u05E1\u05D8 \u05DE\u05D9\u05D5\u05D7\u05D3\u05D9\
  \u05DD, \u05EA\u05D5\u05D5\u05D9\u05DD \u05DC\u05D0 \u05DE\u05D5\u05E2\u05D9\u05DC\
  \u05D9\u05DD \u05D0\u05D5 \u05DE\u05D9\u05D3\u05E2 \u05E9\u05D0\u05D9\u05E0\u05D5\
  \ \u05E8\u05DC\u05D5\u05D5\u05E0\u05D8\u05D9.\u2026"
lastmod: '2024-03-13T22:44:40.174704-06:00'
model: gpt-4-1106-preview
summary: "\u05DE\u05D7\u05D9\u05E7\u05EA \u05EA\u05D5\u05D5\u05D9\u05DD \u05D4\u05EA\
  \u05D5\u05D0\u05DE\u05D9\u05DD \u05DC\u05EA\u05D1\u05E0\u05D9\u05EA \u05D1\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05D4\u05D9\u05D0 \u05E4\u05E2\u05D5\u05DC\u05D4\
  \ \u05E9\u05DE\u05D0\u05E4\u05E9\u05E8\u05EA \u05DC\u05E0\u05E7\u05D5\u05EA \u05DE\
  \u05D9\u05D3\u05E2 \u05DE\u05DC\u05DB\u05DC\u05D5\u05DA \u05D0\u05D5 \u05E4\u05D5\
  \u05E8\u05DE\u05D8 \u05DE\u05D9\u05D5\u05EA\u05E8, \u05DB\u05DE\u05D5 \u05EA\u05D5\
  \u05D5\u05D9 \u05D8\u05E7\u05E1\u05D8 \u05DE\u05D9\u05D5\u05D7\u05D3\u05D9\u05DD\
  , \u05EA\u05D5\u05D5\u05D9\u05DD \u05DC\u05D0 \u05DE\u05D5\u05E2\u05D9\u05DC\u05D9\
  \u05DD \u05D0\u05D5 \u05DE\u05D9\u05D3\u05E2 \u05E9\u05D0\u05D9\u05E0\u05D5 \u05E8\
  \u05DC\u05D5\u05D5\u05E0\u05D8\u05D9."
title: "\u05DE\u05D7\u05D9\u05E7\u05EA \u05EA\u05D5\u05D5\u05D9\u05DD \u05D4\u05EA\
  \u05D5\u05D0\u05DE\u05D9\u05DD \u05DC\u05EA\u05D1\u05E0\u05D9\u05EA"
weight: 5
---

## How to: (איך ל:)
```ruby
# הסבר קצר: נשתמש בשיטה gsub למחיקת תווים
str = "שלום! 123 היי 456 מה קורה? 789"
clean_str = str.gsub(/[0-9]/, '') # מחיקת כל הספרות
puts clean_str # Output: שלום!  היי  מה קורה? 

# גרסה נוספת בשימוש בשיטה delete
more_clean_str = str.delete('0-9')
puts more_clean_str # Output: שלום!  היי  מה קורה? 
```

## Deep Dive (עומק הים):
ב-Ruby, יש כלים רבים לעבודה עם טקסטים. שיטת `gsub` (Global substitution) נוסדה עם רובי עצמה בעוד ש-'delete' היא פשוטה יותר לשימוש אך פחות גמישה. `gsub` מאפשרת להחליף תווים באמצעות ביטויים רגולריים, כך שאפשר למחוק או להחליף תבניות מורכבות יותר ולא רק תווים ספציפיים. השימוש בביטויים רגולריים נעשה נפוץ עלי בסיס ביטויים רגולריים שנפוצו בסביבות פיתוח אחרות והגיעו גם לרובי. הוספת תווים חדשים וביטויים לשפה מאפשרת לטפל במשימות שונות של ניתוח טקסט בצורה יעילה.

## See Also (ראו גם):
- [Ruby's gsub method documentation](https://ruby-doc.org/core-2.7.0/String.html#method-i-gsub)
- [Ruby's delete method documentation](https://ruby-doc.org/core-2.7.0/String.html#method-i-delete)
- [Regular Expressions in Ruby](https://www.rubyguides.com/2015/06/ruby-regex/)
