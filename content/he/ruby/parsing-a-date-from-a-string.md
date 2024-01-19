---
title:                "פענוח תאריך ממחרוזת"
html_title:           "Bash: פענוח תאריך ממחרוזת"
simple_title:         "פענוח תאריך ממחרוזת"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## מה זה ולמה?

ניתוח תאריך ממחרוזת מתייחס לתהליך של פירוק נתונים המגיעים בפורמט מחרוזת לתאריך. זהו פעולה נפוצה בתכנות, המאפשרת למתכניתים להשתמש במגוון המשתנים והמבנים בתוך השפה בנוחות ויעילות.

## איך לעבוד:

ראשית, כדי לנתח את התאריך ממחרוזת, נחליט להשתמש במחלקה Date שבספרייה השכללית של Ruby:

```Ruby
require 'date'

date_string = "2022-03-15"
date = Date.parse(date_string)

puts date.year    # => 2022
puts date.month   # => 3
puts date.day     # => 15
```

## צלילה עמוקה:

כאשר Ruby הופקדה לראשונה, היא כוללת פקודות ליישום פעולות תאריך. דרך אחת לעשות זאת היא באמצעות המחלקה Date, אך  ישנם גם שיטות נוספות כמו השימוש במחלקה Time או DateTime.
להזכיר, כל שיטה מגיעה עם תכונות ואתגרים משלה ומומלץ לבחירה שתתאים לדרישות של המטלה.

## ראה גם:

למידע נוסף אודות איך לעבוד עם תאריכים וזמנים ב-Ruby, בידק/י את המקורות הבאים:

1. [מסמכי Ruby הקדמה לזמנים](https://www.rubyguides.com/ruby-tutorial/times-dates/)
2. [מחלקה דאטה](http://ruby-doc.org/stdlib-2.6.3/libdoc/date/rdoc/Date.html) בתיעוד הרשמי של Ruby.
3. [מחלקה טיים](https://ruby-doc.org/core-2.6.3/Time.html) בתיעוד הרשמי של Ruby.