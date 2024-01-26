---
title:                "ניתוח תאריך ממחרוזת"
date:                  2024-01-20T15:38:41.574445-07:00
html_title:           "Arduino: ניתוח תאריך ממחרוזת"
simple_title:         "ניתוח תאריך ממחרוזת"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
פריסת תאריך ממחרוזת היא תהליך שבו הופכים טקסט לאובייקט תאריך. פיתוחים לעושים את זה כדי לנהל בצורה יעילה תאריכים בתוכנות.

## איך לעשות:
```ruby
require 'date'

# פריסת מחרוזת לאובייקט תאריך
date_string = "2023-03-14"
parsed_date = Date.parse(date_string)
puts parsed_date
# => 2023-03-14

# השתמש ב-strptime כאשר יש פורמט מותאם אישית
custom_format_date_string = "14/03/2023"
custom_format_parsed_date = Date.strptime(custom_format_date_string, '%d/%m/%Y')
puts custom_format_parsed_date
# => 2023-03-14
```

## עומק הצלילה
ב-Ruby, פריסת תאריך ממחרוזת היא משהו די ישיר עם הגמ"ח 'date'. קודם לגרסת 1.9, מרבית הפיתוחים השתמשו בספריה 'time' או ספריות צד שלישי, כמו 'chronic'. ישנם גם חלופות מודרניות כמו 'active_support' ב-Rails, שמספק עוד יכולות פריסה וניהול תאריכים. עמידה בפני שגיאות ניתוח היא חשובה; לכן זה עשוי להיות חכם להשתמש ב-'begin-rescue' כדי להתמודד עם תאריכים בלתי תקניים.

## ראו גם
- [דוקומנטציה רשמית של Date](https://ruby-doc.org/stdlib-2.7.0/libdoc/date/rdoc/Date.html)
- [מדריך לניהול זמן ותאריכים ב-Ruby](https://www.rubyguides.com/2015/12/ruby-time/)
- [קהילת StackOverflow על ניתוח תאריכים](https://stackoverflow.com/questions/tagged/ruby+date-parsing)
