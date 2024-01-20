---
title:                "השוואה בין שני תאריכים"
html_title:           "Arduino: השוואה בין שני תאריכים"
simple_title:         "השוואה בין שני תאריכים"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## מה זה ולמה? 
השוואת שני תאריכים היא פעולת התמחושה של ההבדל בין שני תאריכים. מתכנתים מבצעים את זה כדי לדעת את מספר הימים, השעות או הדקות בין שני נקודות זמן נתונות.

## כיצד לבצע:
נכון לרובי (גרסה נוכחית), ניתן להשתמש במחלקת 'זמן' כדי להשוות בין שני תאריכים. והנה כיצד זה נעשה:
```ruby
require 'time'

date1 = Time.parse('2022-01-01')
date2 = Time.parse('2022-01-02')

difference_in_days = (date2 - date1) / (60 * 60 * 24)
puts difference_in_days
```
בריצת הקוד הזה תקבל פלט של `1.0`, משמע, ישנם יום אחד בין התאריכים.

## צלילה עמוקה
מאז היווסרה שפת Ruby, השוואת תאריכים היא פונקציה שהיא חלק ממחלקת 'Time'. זו המחלקה שמאפשרת לך לעבוד עם זמנים ותאריכים. חלופות נוספות כוללות את מחלקת 'Date' ומחלקת 'DateTime', אך 'Time' היא הראויה לחלופה הפשוטה ביותר. מאחר ו-Ruby היא שפת מדעי המחשב שהאריכה את תורתה לפני מספר שנים, זו המחלקה שמספקת את הדרך המורה קו לעבוד עם זמנים ותאריכים.

## ראה גם
* [מדריך ה-Ruby הרשמי לעבודה עם זמן dan-תאריכים](https://ruby-doc.org/stdlib-2.5.1/libdoc/date/rdoc/Date.html)