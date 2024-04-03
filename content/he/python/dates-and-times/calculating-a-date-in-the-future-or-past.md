---
date: 2024-01-20 17:32:11.038626-07:00
description: "\u05D7\u05D9\u05E9\u05D5\u05D1 \u05EA\u05D0\u05E8\u05D9\u05DA \u05D1\
  \u05E2\u05EA\u05D9\u05D3 \u05D0\u05D5 \u05D1\u05E2\u05D1\u05E8 \u05D6\u05D4 \u05E4\
  \u05E9\u05D5\u05D8 \u05DC\u05E7\u05D1\u05D5\u05E2 \u05EA\u05D0\u05E8\u05D9\u05DA\
  \ \u05E9\u05D9\u05D4\u05D9\u05D4 \u05D0\u05D5 \u05E9\u05D4\u05D9\u05D4 \u05DC\u05E4\
  \u05E0\u05D9 \u05DE\u05E1\u05E4\u05E8 \u05DE\u05E1\u05D5\u05D9\u05DD \u05E9\u05DC\
  \ \u05D9\u05DE\u05D9\u05DD, \u05E9\u05D1\u05D5\u05E2\u05D5\u05EA, \u05D7\u05D5\u05D3\
  \u05E9\u05D9\u05DD \u05D0\u05D5 \u05E9\u05E0\u05D9\u05DD \u05DE\u05D4\u05D9\u05D5\
  \u05DD. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\
  \u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05E0\u05D4\u05DC \u05D0\u05D9\u05E8\
  \u05D5\u05E2\u05D9\u05DD\u2026"
lastmod: '2024-03-13T22:44:38.657750-06:00'
model: gpt-4-1106-preview
summary: "\u05D7\u05D9\u05E9\u05D5\u05D1 \u05EA\u05D0\u05E8\u05D9\u05DA \u05D1\u05E2\
  \u05EA\u05D9\u05D3 \u05D0\u05D5 \u05D1\u05E2\u05D1\u05E8 \u05D6\u05D4 \u05E4\u05E9\
  \u05D5\u05D8 \u05DC\u05E7\u05D1\u05D5\u05E2 \u05EA\u05D0\u05E8\u05D9\u05DA \u05E9\
  \u05D9\u05D4\u05D9\u05D4 \u05D0\u05D5 \u05E9\u05D4\u05D9\u05D4 \u05DC\u05E4\u05E0\
  \u05D9 \u05DE\u05E1\u05E4\u05E8 \u05DE\u05E1\u05D5\u05D9\u05DD \u05E9\u05DC \u05D9\
  \u05DE\u05D9\u05DD, \u05E9\u05D1\u05D5\u05E2\u05D5\u05EA, \u05D7\u05D5\u05D3\u05E9\
  \u05D9\u05DD \u05D0\u05D5 \u05E9\u05E0\u05D9\u05DD \u05DE\u05D4\u05D9\u05D5\u05DD\
  ."
title: "\u05D7\u05D9\u05E9\u05D5\u05D1 \u05EA\u05D0\u05E8\u05D9\u05DA \u05D1\u05E2\
  \u05EA\u05D9\u05D3 \u05D0\u05D5 \u05D1\u05E2\u05D1\u05E8"
weight: 26
---

## מה ולמה?
חישוב תאריך בעתיד או בעבר זה פשוט לקבוע תאריך שיהיה או שהיה לפני מספר מסוים של ימים, שבועות, חודשים או שנים מהיום. תכנתים עושים את זה כדי לנהל אירועים עתידיים, לקבוע תזכורות, או לפקח על תוקף.

## איך לעשות:
```Python
from datetime import datetime, timedelta

# היום
today = datetime.now()
print("היום:", today.strftime("%d/%m/%Y"))

# חישוב תאריך בעתיד – 10 ימים מהיום
future_date = today + timedelta(days=10)
print("עוד 10 ימים:", future_date.strftime("%d/%m/%Y"))

# חישוב תאריך בעבר – 30 ימים לפני היום
past_date = today - timedelta(days=30)
print("לפני 30 ימים:", past_date.strftime("%d/%m/%Y"))
```

דוגמא לפלט:
```
היום: 05/04/2023
עוד 10 ימים: 15/04/2023
לפני 30 ימים: 06/03/2023
```

## עיון נוסף:
לפני שהייתה מחלקת `datetime`, תכנתים היו צריכים לחשב תאריכים בצורה ידנית – תהליך מסורבל ומסובך. אלטרנטיבות כוללות ספריות כמו `dateutil` שמאפשרת גמישות גדולה יותר בניהול תאריכים. המימוש ב `datetime` מתבסס על משתנים מהסוג `timedelta`, שמאפשרים ייצוג של פרקי זמן והוספה או הפחתה מתאריכים.

## לקרוא גם:
- תיעוד המודול `datetime` הרשמי של פייתון: https://docs.python.org/3/library/datetime.html
- מדריך לספריית `dateutil`: https://dateutil.readthedocs.io/en/stable/
- סקירה על טיפול בזמנים ותאריכים בפייתון: https://realpython.com/python-datetime/
