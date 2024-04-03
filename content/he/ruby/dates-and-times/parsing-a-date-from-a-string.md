---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:43.936504-07:00
description: "\u05E4\u05D9\u05E2\u05E0\u05D5\u05D7 \u05EA\u05D0\u05E8\u05D9\u05DA\
  \ \u05DE\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D4\u05D5\u05D0 \u05E2\u05DC \u05D4\
  \u05DE\u05E8\u05EA \u05D8\u05E7\u05E1\u05D8 \u05E9\u05DE\u05D9\u05D9\u05E6\u05D2\
  \ \u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\u05D0\u05D5\u05D1\u05D9\u05D9\u05E7\u05D8\
  \ \u05DE\u05E1\u05D5\u05D2 `Date` \u05D0\u05D5 `DateTime` \u05E9\u05E8\u05D5\u05D1\
  \u05D9 \u05DE\u05D1\u05D9\u05E0\u05D4. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\
  \u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05E2\u05DC \u05DE\u05E0\u05EA \u05DC\
  \u05D1\u05E6\u05E2 \u05E4\u05E2\u05D5\u05DC\u05D5\u05EA \u05DB\u05DE\u05D5 \u05D4\
  \u05E9\u05D5\u05D5\u05D0\u05D5\u05EA,\u2026"
lastmod: '2024-03-13T22:44:40.220324-06:00'
model: gpt-4-0125-preview
summary: "\u05E4\u05D9\u05E2\u05E0\u05D5\u05D7 \u05EA\u05D0\u05E8\u05D9\u05DA \u05DE\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D4\u05D5\u05D0 \u05E2\u05DC \u05D4\u05DE\
  \u05E8\u05EA \u05D8\u05E7\u05E1\u05D8 \u05E9\u05DE\u05D9\u05D9\u05E6\u05D2 \u05EA\
  \u05D0\u05E8\u05D9\u05DA \u05DC\u05D0\u05D5\u05D1\u05D9\u05D9\u05E7\u05D8 \u05DE\
  \u05E1\u05D5\u05D2 `Date` \u05D0\u05D5 `DateTime` \u05E9\u05E8\u05D5\u05D1\u05D9\
  \ \u05DE\u05D1\u05D9\u05E0\u05D4."
title: "\u05E4\u05E8\u05E1\u05D5\u05DD \u05EA\u05D0\u05E8\u05D9\u05DA \u05DE\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA"
weight: 30
---

## איך לעשות:
ברובי, הספרייה הסטנדרטית מספקת דרכים ישירות לפענח תאריכים ממחרוזות באמצעות הכיתות `Date` ו`DateTime`. הנה איך עושים את זה באמצעות שיטות המובנות של רובי:

```ruby
require 'date'

# פיענוח תאריך ממחרוזת
date_string = "2023-04-01"
parsed_date = Date.parse(date_string)
puts parsed_date
# => 2023-04-01

# DateTime לייצוג זמן מדויק יותר
datetime_string = "2023-04-01T15:30:45+00:00"
parsed_datetime = DateTime.parse(datetime_string)
puts parsed_datetime
# => 2023-04-01T15:30:45+00:00
```

לשליטה רבה יותר או לטיפול בפורמטים שה`parse` אולי לא תבין באופן ישיר, ניתן להשתמש ב`strptime` (פיענוח זמן ממחרוזת), תוך ציון הפורמט במפורש:

```ruby
# שימוש בstrptime לפורמטים מותאמים אישית
custom_date_string = "01-04-2023"
parsed_date_custom = Date.strptime(custom_date_string, '%d-%m-%Y')
puts parsed_date_custom
# => 2023-04-01
```

### שימוש בספריות צד שלישי:
אף על פי שהיכולות המובנות של רובי חזקות, לעיתים יתכן שתעדיפו ספריות צד שלישי לקבלת תכונות נוספות או תחביר פשוט יותר. בחירה פופולרית היא הגביש `Chronic` עבור פיענוח שפת טבע:

1. תחילה, הוסיפו את Chronic לקובץ Gemfile שלכם והריצו `bundle install`:
```ruby
gem 'chronic'
```

2. לאחר מכן, השתמשו בו כך:
```ruby
require 'chronic'

parsed_chronic = Chronic.parse('next Tuesday')
puts parsed_chronic
# הפלט ישתנה בהתאם לתאריך הנוכחי; נחשב על סמך פיענוח ב-2023-04-01
# => 2023-04-04 12:00:00 +0000
```

`Chronic` מאוד שימושי לקלט משתמש מכיוון שהוא מסוגל להבין מגוון רחב של פורמטי תאריך בשפת טבע, מה שהופך אותו לכלי חזק ליישומים הדורשים כניסת תאריך גמישה.
