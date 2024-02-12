---
title:                "קבלת התאריך הנוכחי"
aliases:
- /he/ruby/getting-the-current-date/
date:                  2024-02-03T19:10:56.746625-07:00
model:                 gpt-4-0125-preview
simple_title:         "קבלת התאריך הנוכחי"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
איסוף התאריך הנוכחי הוא משימה חיונית בכמעט כל מפעל תכנותי, החל מרישום פעולות ביישום וכלה ביצירת דוחות עם חותמות תאריך. בשפת Ruby, ניתן לבצע זאת בקלות באמצעות הספרייה הסטנדרטית, מה שמפשט פעולות הקשורות לתאריכים.

## איך לעשות:
הספרייה הסטנדרטית של Ruby כוללת את המחלקות `Date` ו-`Time` לטיפול בתאריכים ובזמן. הנה איך לקבל את התאריך הנוכחי:

```ruby
require 'date'

current_date = Date.today
puts current_date
```

פלט לדוגמה:
```
2023-04-12
```

לכלול זמן עם התאריך, המחלקה `Time` של Ruby מתאימה יותר:

```ruby
current_time = Time.now
puts current_time
```

פלט לדוגמה:
```
2023-04-12 14:33:07 +0200
```

אם אתם זקוקים לפונקציונליות נוספת, כמו ניהול אזורי זמן, כדאי לכם להשתמש בגמיש צד שלישי כמו `ActiveSupport` (חלק מ-Rails אך ניתן לשימוש עצמאי).

ראשית, הוסיפו את `activesupport` לקובץ ה-Gemfile שלכם והריצו `bundle install`:

```ruby
gem 'activesupport'
```

לאחר מכן, השתמשו בו לניהול אזורי זמן:

```ruby
require 'active_support/time'

Time.zone = 'Eastern Time (US & Canada)'  # קבעו את אזור הזמן המבוקש שלכם
current_time_with_zone = Time.zone.now
puts current_time_with_zone
```

פלט לדוגמה:
```
Wed, 12 Apr 2023 08:33:07 EDT -04:00
```
