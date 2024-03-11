---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:56.746625-07:00
description: "\u05D0\u05D9\u05E1\u05D5\u05E3 \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA\
  \ \u05D4\u05E0\u05D5\u05DB\u05D7\u05D9 \u05D4\u05D5\u05D0 \u05DE\u05E9\u05D9\u05DE\
  \u05D4 \u05D7\u05D9\u05D5\u05E0\u05D9\u05EA \u05D1\u05DB\u05DE\u05E2\u05D8 \u05DB\
  \u05DC \u05DE\u05E4\u05E2\u05DC \u05EA\u05DB\u05E0\u05D5\u05EA\u05D9, \u05D4\u05D7\
  \u05DC \u05DE\u05E8\u05D9\u05E9\u05D5\u05DD \u05E4\u05E2\u05D5\u05DC\u05D5\u05EA\
  \ \u05D1\u05D9\u05D9\u05E9\u05D5\u05DD \u05D5\u05DB\u05DC\u05D4 \u05D1\u05D9\u05E6\
  \u05D9\u05E8\u05EA \u05D3\u05D5\u05D7\u05D5\u05EA \u05E2\u05DD \u05D7\u05D5\u05EA\
  \u05DE\u05D5\u05EA \u05EA\u05D0\u05E8\u05D9\u05DA. \u05D1\u05E9\u05E4\u05EA Ruby,\
  \ \u05E0\u05D9\u05EA\u05DF \u05DC\u05D1\u05E6\u05E2 \u05D6\u05D0\u05EA \u05D1\u05E7\
  \u05DC\u05D5\u05EA\u2026"
lastmod: '2024-03-11T00:14:13.733656-06:00'
model: gpt-4-0125-preview
summary: "\u05D0\u05D9\u05E1\u05D5\u05E3 \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05D4\
  \u05E0\u05D5\u05DB\u05D7\u05D9 \u05D4\u05D5\u05D0 \u05DE\u05E9\u05D9\u05DE\u05D4\
  \ \u05D7\u05D9\u05D5\u05E0\u05D9\u05EA \u05D1\u05DB\u05DE\u05E2\u05D8 \u05DB\u05DC\
  \ \u05DE\u05E4\u05E2\u05DC \u05EA\u05DB\u05E0\u05D5\u05EA\u05D9, \u05D4\u05D7\u05DC\
  \ \u05DE\u05E8\u05D9\u05E9\u05D5\u05DD \u05E4\u05E2\u05D5\u05DC\u05D5\u05EA \u05D1\
  \u05D9\u05D9\u05E9\u05D5\u05DD \u05D5\u05DB\u05DC\u05D4 \u05D1\u05D9\u05E6\u05D9\
  \u05E8\u05EA \u05D3\u05D5\u05D7\u05D5\u05EA \u05E2\u05DD \u05D7\u05D5\u05EA\u05DE\
  \u05D5\u05EA \u05EA\u05D0\u05E8\u05D9\u05DA. \u05D1\u05E9\u05E4\u05EA Ruby, \u05E0\
  \u05D9\u05EA\u05DF \u05DC\u05D1\u05E6\u05E2 \u05D6\u05D0\u05EA \u05D1\u05E7\u05DC\
  \u05D5\u05EA\u2026"
title: "\u05E7\u05D1\u05DC\u05EA \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05D4\u05E0\
  \u05D5\u05DB\u05D7\u05D9"
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
