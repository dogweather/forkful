---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:29.998679-07:00
description: "\u05E0\u05D9\u05EA\u05D5\u05D7 \u05EA\u05D0\u05E8\u05D9\u05DA \u05DE\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DB\u05D5\u05DC\u05DC \u05D4\u05DE\u05E8\
  \u05EA \u05DE\u05D9\u05D3\u05E2 \u05D8\u05E7\u05E1\u05D8\u05D5\u05D0\u05DC\u05D9\
  \ \u05E2\u05DC \u05EA\u05D0\u05E8\u05D9\u05DA \u05D5\u05E9\u05E2\u05D4 \u05DC\u05D0\
  \u05D5\u05D1\u05D9\u05D9\u05E7\u05D8 datetime \u05D0\u05D5 \u05DC\u05E4\u05D5\u05E8\
  \u05DE\u05D8 \u05DE\u05D5\u05D1\u05E0\u05D4 \u05D3\u05D5\u05DE\u05D4. \u05E4\u05E2\
  \u05D5\u05DC\u05D4 \u05D6\u05D5 \u05E0\u05E2\u05E9\u05D9\u05EA \u05DC\u05E8\u05D5\
  \u05D1 \u05DB\u05D3\u05D9 \u05DC\u05D0\u05E4\u05E9\u05E8 \u05E4\u05E2\u05D5\u05DC\
  \u05D5\u05EA \u05D7\u05D9\u05E9\u05D5\u05D1 \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\
  \u05DD,\u2026"
lastmod: '2024-03-13T22:44:38.651384-06:00'
model: gpt-4-0125-preview
summary: "\u05E0\u05D9\u05EA\u05D5\u05D7 \u05EA\u05D0\u05E8\u05D9\u05DA \u05DE\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05DB\u05D5\u05DC\u05DC \u05D4\u05DE\u05E8\u05EA\
  \ \u05DE\u05D9\u05D3\u05E2 \u05D8\u05E7\u05E1\u05D8\u05D5\u05D0\u05DC\u05D9 \u05E2\
  \u05DC \u05EA\u05D0\u05E8\u05D9\u05DA \u05D5\u05E9\u05E2\u05D4 \u05DC\u05D0\u05D5\
  \u05D1\u05D9\u05D9\u05E7\u05D8 datetime \u05D0\u05D5 \u05DC\u05E4\u05D5\u05E8\u05DE\
  \u05D8 \u05DE\u05D5\u05D1\u05E0\u05D4 \u05D3\u05D5\u05DE\u05D4."
title: "\u05E4\u05E8\u05E1\u05D5\u05DD \u05EA\u05D0\u05E8\u05D9\u05DA \u05DE\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA"
weight: 30
---

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
