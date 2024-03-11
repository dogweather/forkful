---
date: 2024-01-20 17:33:50.261722-07:00
description: "\u05D4\u05E9\u05D5\u05D5\u05D0\u05EA \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\
  \u05DD \u05D1\u05E4\u05D9\u05D9\u05EA\u05D5\u05DF \u05D6\u05D4 \u05E4\u05E9\u05D5\
  \u05D8 \u05DC\u05D1\u05D3\u05D5\u05E7 \u05D0\u05D9\u05D6\u05D4 \u05EA\u05D0\u05E8\
  \u05D9\u05DA \u05E7\u05D3\u05DD \u05DC\u05D0\u05D9\u05D6\u05D4, \u05D0\u05D5 \u05D0\
  \u05DD \u05D4\u05DD \u05D6\u05D4\u05D9\u05DD. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\
  \u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DC\u05DE\u05E9\u05DC\
  \ \u05DB\u05D3\u05D9 \u05DC\u05D7\u05E9\u05D1 \u05E4\u05E8\u05E7\u05D9 \u05D6\u05DE\
  \u05DF, \u05DC\u05D0\u05DE\u05EA \u05EA\u05D5\u05E7\u05E3 \u05D5\u05E2\u05D5\u05D3\
  ."
lastmod: '2024-03-11T00:14:12.072453-06:00'
model: gpt-4-1106-preview
summary: "\u05D4\u05E9\u05D5\u05D5\u05D0\u05EA \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\
  \u05DD \u05D1\u05E4\u05D9\u05D9\u05EA\u05D5\u05DF \u05D6\u05D4 \u05E4\u05E9\u05D5\
  \u05D8 \u05DC\u05D1\u05D3\u05D5\u05E7 \u05D0\u05D9\u05D6\u05D4 \u05EA\u05D0\u05E8\
  \u05D9\u05DA \u05E7\u05D3\u05DD \u05DC\u05D0\u05D9\u05D6\u05D4, \u05D0\u05D5 \u05D0\
  \u05DD \u05D4\u05DD \u05D6\u05D4\u05D9\u05DD. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\
  \u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DC\u05DE\u05E9\u05DC\
  \ \u05DB\u05D3\u05D9 \u05DC\u05D7\u05E9\u05D1 \u05E4\u05E8\u05E7\u05D9 \u05D6\u05DE\
  \u05DF, \u05DC\u05D0\u05DE\u05EA \u05EA\u05D5\u05E7\u05E3 \u05D5\u05E2\u05D5\u05D3\
  ."
title: "\u05D4\u05E9\u05D5\u05D5\u05D0\u05EA \u05E9\u05EA\u05D9 \u05EA\u05D0\u05E8\
  \u05D9\u05DB\u05D9\u05DD"
---

{{< edit_this_page >}}

## מה ולמה?
השוואת תאריכים בפייתון זה פשוט לבדוק איזה תאריך קדם לאיזה, או אם הם זהים. מתכנתים עושים זאת למשל כדי לחשב פרקי זמן, לאמת תוקף ועוד.

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
