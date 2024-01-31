---
title:                "קבלת התאריך הנוכחי"
date:                  2024-01-20T15:16:27.851337-07:00
html_title:           "C: קבלת התאריך הנוכחי"
simple_title:         "קבלת התאריך הנוכחי"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## מה ולמה?
קבלת התאריך הנוכחי בפייתון היא אחת הפעולות הבסיסיות בתכנות. מתכנתים עושים זאת כדי לתייג אירועים, לחשב תקופות זמן ולהתאים תוכנה להגדרות מקומיות של זמן.

## איך לעשות:
קוד Python פשוט שמראה כיצד להשיג את התאריך הנוכחי:

```Python
from datetime import datetime

# קבלת התאריך והשעה הנוכחיים
now = datetime.now()

# הדפסת התאריך והשעה הנוכחיים
print("התאריך והשעה הנוכחיים הם:", now)

# אם רק התאריך הנוכחי מעניין אותך
today = datetime.today().date()
print("התאריך הנוכחי הוא:", today)
```

דוגמא לפלט:

```
התאריך והשעה הנוכחיים הם: 2023-03-20 17:45:03.417260
התאריך הנוכחי הוא: 2023-03-20
```

## צלילה לעומק:
`datetime` הוא מודול בספריית התקנית של פייתון שמטפל בתאריכים וזמנים. כבר מימי Python 2.x, המודול הזה עזר למתכנתים לנהל בצורה יעילה נתוני תאריך ושעה. בנוסף למודול `datetime`, ישנן ספריות צד שלישי כמו `dateutil` ו`pytz` שמרחיבות את האפשרויות לעבודה עם נתוני זמן מורכבים יותר, כגון חישוב חגים וניהול זמנים עולמיים.

## ראה גם:
- תיעוד רשמי של `datetime`: https://docs.python.org/3/library/datetime.html
- ספריית `pytz` לעבודה עם אזורי זמן: https://pypi.org/project/pytz/
- ספריית `dateutil` לחישובים מרובים של תאריכים: https://pypi.org/project/python-dateutil/
