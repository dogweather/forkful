---
title:                "פרסום תאריך ממחרוזת"
aliases: - /he/python/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:15:29.998679-07:00
model:                 gpt-4-0125-preview
simple_title:         "פרסום תאריך ממחרוזת"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
ניתוח תאריך ממחרוזת כולל המרת מידע טקסטואלי על תאריך ושעה לאובייקט datetime או לפורמט מובנה דומה. פעולה זו נעשית לרוב כדי לאפשר פעולות חישוב תאריכים, השוואות ועיצוב בדרך שאינה תלוית שפה או אזור. מתכנתים עושים זאת כדי לטפל ולתפעל נתונים זמניים שמופקים מלוגים, קלטי משתמשים או מקורות חיצוניים ביעילות.

## איך לעשות:
ספריית הסטנדרט של פייתון מספקת את המודול `datetime`, הכולל את המתודה `strptime` למטרה זו. המתודה דורשת שני ארגומנטים: מחרוזת התאריך ודירקטיבת פורמט שמציינת את תבנית המחרוזת הקלט.

```python
from datetime import datetime

# מחרוזת לדוגמה
date_string = "2023-04-01 14:30:00"
# ניתוח מחרוזת לאובייקט datetime
parsed_date = datetime.strptime(date_string, "%Y-%m-%d %H:%M:%S")

print(parsed_date)
# פלט: 2023-04-01 14:30:00
```

לניתוח תאריכים עם דרישות יותר מורכבות, במיוחד כאשר מתמודדים עם מספר פורמטים או לוקאלים, הספריה החיצונית `dateutil` יכולה להיות מאוד מועילה. היא מספקת מודול parser אשר יכול לנתח תאריכים בכמעט כל פורמט מחרוזת.

```python
from dateutil import parser

# מחרוזות לדוגמה
date_string1 = "April 1, 2023 2:30 PM"
date_string2 = "1st April 2023 14:30"

# שימוש ב-parser של dateutil
parsed_date1 = parser.parse(date_string1)
parsed_date2 = parser.parse(date_string2)

print(parsed_date1)
# פלט: 2023-04-01 14:30:00
print(parsed_date2)
# פלט: 2023-04-01 14:30:00
```

`dateutil` יכול להתמודד עם רוב פורמטי התאריך ללא מחרוזות פורמט מפורשות, דבר ההופך אותו לבחירה גמישה ליישומים המתמודדים עם הצגות תאריך מגוונות.
