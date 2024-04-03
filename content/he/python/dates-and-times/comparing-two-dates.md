---
date: 2024-01-20 17:33:50.261722-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D9\u05D1\u05D5\
  \u05D0 \u05D4\u05D7\u05D1\u05D9\u05DC\u05D9\u05DD \u05D4\u05E0\u05D7\u05D5\u05E6\
  \u05D9\u05DD \u05D5\u05D4\u05E9\u05D5\u05D5\u05D0\u05D4 \u05D1\u05E1\u05D9\u05E1\
  \u05D9\u05EA."
lastmod: '2024-03-13T22:44:38.656216-06:00'
model: gpt-4-1106-preview
summary: "\u05D9\u05D1\u05D5\u05D0 \u05D4\u05D7\u05D1\u05D9\u05DC\u05D9\u05DD \u05D4\
  \u05E0\u05D7\u05D5\u05E6\u05D9\u05DD \u05D5\u05D4\u05E9\u05D5\u05D5\u05D0\u05D4\
  \ \u05D1\u05E1\u05D9\u05E1\u05D9\u05EA."
title: "\u05D4\u05E9\u05D5\u05D5\u05D0\u05EA \u05E9\u05EA\u05D9 \u05EA\u05D0\u05E8\
  \u05D9\u05DB\u05D9\u05DD"
weight: 27
---

## איך לעשות:
יבוא החבילים הנחוצים והשוואה בסיסית:

```Python
from datetime import datetime

# הגדרת שני תאריכים
date1 = datetime(2023, 3, 5)
date2 = datetime(2023, 4, 15)

# השוואת תאריכים
if date1 < date2:
    print("date1 קודם ל-date2")
elif date1 > date2:
    print("date1 אחרי date2")
else:
    print("התאריכים זהים")

# דוגמת פלט:
# date1 קודם ל-date2
```

בדיקת הפרש זמנים:

```Python
# חישוב הפרש זמן בין שני תאריכים
difference = date2 - date1

# הדפסת ההפרש
print(f"ההפרש הוא: {difference.days} ימים")

# דוגמת פלט:
# ההפרש הוא: 41 ימים
```

## צלילה לעומק
במחשב, תאריכים מיוצגים כמספרים שמתארים כמה זמן עבר מנקודת התחלה קבועה בזמן, למשל ינואר 1, 1970 ב-UTC. ב-Python, המודול `datetime` מאפשר השוואה בצורה פשוטה בין אובייקטי תאריך וזמן, כאילו הם מספרים.

ישנם אלטרנטיבות כמו חבילת `dateutil` המספקת פונקציונליות נוספת וטיפול במקרי קצה יותר מורכבים.

הקפידו תמיד להתייחס לאזורי זמן ולקיץ שעון חורף, כדי להימנע מטעויות בחישובים.

## ראו גם
- מסמך התיעוד של המודול `datetime`: https://docs.python.org/3/library/datetime.html
- חבילת `dateutil` עבור טיפול מתקדם בתאריכים: https://pypi.org/project/python-dateutil/
- על מספור תאריכים וזמנים (Unix Time): https://en.wikipedia.org/wiki/Unix_time
