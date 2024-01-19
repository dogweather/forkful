---
title:                "קבלת התאריך הנוכחי"
html_title:           "C#: קבלת התאריך הנוכחי"
simple_title:         "קבלת התאריך הנוכחי"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

# מה ולמה?

מאמץ דייט מתודה מהירה ונוחה שמאפשרת לך לקבל את התאריך הנוכחי. תכנתים לעיתים עשויים להשתמש בו ללגוג אירועים, נתונים כחלק מכלים שלנו, לשם גביית מידע בזמן שמתרחש.

# איך:

```Ruby
require 'date'

current_date = Date.today
puts current_date
```

ניתן לצפות בפלט:

```Ruby
2022-03-01
```

# מעמיקים:

**הקשר ההיסטורי**: בעבר, נאלצנו להשתמש בתכנות מורכבת ומעיקה כדי לקבל את התאריך והשעה הנוכחיים. עם הגעת Ruby, קבלנו כלים אינטואטיביים גם לצורך זה.

**החלופות**: `DateTime.now` הוא עוד דרך פופולרית לקבלת התאריך הנוכחי.

```Ruby
require 'date'

current_date_time = DateTime.now
puts current_date_time
```

**פרטי ביצוע**: המתודה `Date.today` משתמשת בפונקציית המערכת הליבה של Ruby בשם `gettimeofday` כדי לספק את התאריך הנוכחי. 

# ראה גם:

- [Date - Ruby 2.7.0](https://ruby-doc.org/stdlib-2.7.0/libdoc/date/rdoc/Date.html)
- [DateTime - Ruby 2.7.0](https://ruby-doc.org/stdlib-2.7.0/libdoc/date/rdoc/DateTime.html)
- [הדרך הנכונה להשיג את התאריך והשעה הנוכחיים ב-Ruby](https://stackoverflow.com/questions/3240085/what-is-the-best-way-to-get-the-current-date-time-in-ruby)