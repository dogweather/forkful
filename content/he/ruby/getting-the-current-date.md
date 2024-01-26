---
title:                "קבלת התאריך הנוכחי"
date:                  2024-01-20T15:16:34.322023-07:00
html_title:           "C: קבלת התאריך הנוכחי"
simple_title:         "קבלת התאריך הנוכחי"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## מה ולמה?
השגת התאריך הנוכחי ב-Ruby זה פשוט לפקודה שמחזירה את התאריך והשעה הנוכחיים. מתכנתים עושים את זה ללוגים, תיעוד, תזמונים, ועוד.

## איך לעשות:
קטעי קוד ודוגמאות פלט בתוך בלוקי הקוד ```Ruby ... ```:

```Ruby
require 'date'

# השגת התאריך והשעה הנוכחיים
current_datetime = DateTime.now
puts current_datetime
# 2023-04-05T15:27:35+03:00

# רק התאריך הנוכחי
current_date = Date.today
puts current_date
# 2023-04-05

# רק השעה הנוכחית
current_time = Time.now
puts current_time.strftime("%H:%M:%S")
# 15:27:35
```

## עיון נוסף:
פרטים נוספים כגון (1) הקשר ההיסטורי, (2) אלטרנטיבות, ו-(3) פרטי היישום לגבי השגת התאריך הנוכחי.

ב-Ruby, המודול `Date` והמחלקה `Time` הן חלק מסטנדרט התכנות של השפה. זה לא תמיד היה ככה; מקודם, תאריכים ושעות היו יותר מסובכים להשיג ולנהל.

אלטרנטיבות כוללות ספריות צד שלישי כמו 'Chronic' לפרסינג טקסט לתאריכים, או 'ActiveSupport' שמקל על עבודה עם תאריכים במיוחד ב-Rails.

בקרבת חצות, זהירות עם השימוש ב-`Date.today` מאחר שיכולים להיות מקרים של אי-תאמה עם השעה המדויקת שמחפשים.

## ראה גם:
- [Ruby Time Documentation](https://ruby-doc.org/core/Time.html)
- [ActiveSupport Core Extensions](https://guides.rubyonrails.org/active_support_core_extensions.html)
