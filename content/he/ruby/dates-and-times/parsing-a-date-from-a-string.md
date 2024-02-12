---
title:                "פרסום תאריך ממחרוזת"
aliases: - /he/ruby/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:15:43.936504-07:00
model:                 gpt-4-0125-preview
simple_title:         "פרסום תאריך ממחרוזת"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
פיענוח תאריך ממחרוזת הוא על המרת טקסט שמייצג תאריך לאובייקט מסוג `Date` או `DateTime` שרובי מבינה. תכנתים עושים זאת על מנת לבצע פעולות כמו השוואות, חישובים, או עיצוב של תאריכים, אשר הם משימות נפוצות ביישומים העוסקים בתכנון, אנליטיקה, או עיבוד נתונים.

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
