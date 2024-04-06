---
date: 2024-01-20 17:32:15.929412-07:00
description: "\u05D0\u05D9\u05DA \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\
  \u05D4: \u05D3\u05D5\u05D2\u05DE\u05D0\u05EA \u05E4\u05DC\u05D8."
lastmod: '2024-04-05T21:53:41.210477-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u05D7\u05D9\u05E9\u05D5\u05D1 \u05EA\u05D0\u05E8\u05D9\u05DA \u05D1\u05E2\
  \u05EA\u05D9\u05D3 \u05D0\u05D5 \u05D1\u05E2\u05D1\u05E8"
weight: 26
---

## איך עושים את זה:
```Ruby
require 'date'

# מוסיפים ימים לתאריך היום
future_date = Date.today + 10
puts "עוד עשרה ימים יהיה: #{future_date}"

# לקחת ימים מתאריך היום
past_date = Date.today - 10
puts "לפני עשרה ימים היה: #{past_date}"
```
דוגמאת פלט:
```
עוד עשרה ימים יהיה: 2023-04-20
לפני עשרה ימים היה: 2023-03-31
```

## צלילה לעומק
כלי חישובי תאריכים הם תוספת מאוחרת לשפות תכנות. גירסאות קדומות של Ruby לא כללו מערכת נוחה לטיפול בתאריכים וזמנים. כיום, מודולים כמו `date` ו-`time` הם חלק בלתי נפרד מהסטנדרט של Ruby. אפשרויות אלטרנטיביות כוללות את הג'מים `active_support` מסביבת ה-Rails, המרחיבה את יכולות עבודה עם תאריכים וזמנים. לטיפול במקרים מורכבים יותר, ולחישובי זמנים מדויקים, לעתים משתמשים ב־APIs חיצוניים או בספריות כמו 'Chronic' שמאפשרים פרשנות של מחרוזות טבעיות לתאריכים.

## קישורים למידע נוסף:
- [תיעוד המודול Date](https://ruby-doc.org/stdlib-3.1.0/libdoc/date/rdoc/Date.html)
- [Rails Active Support Core Extensions](https://edgeguides.rubyonrails.org/active_support_core_extensions.html)
- [Chronic - gem לפרשנות תאריכים טבעיים](https://github.com/mojombo/chronic)
