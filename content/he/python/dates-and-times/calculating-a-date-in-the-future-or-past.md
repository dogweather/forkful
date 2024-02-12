---
title:                "חישוב תאריך בעתיד או בעבר"
aliases:
- /he/python/calculating-a-date-in-the-future-or-past/
date:                  2024-01-20T17:32:11.038626-07:00
model:                 gpt-4-1106-preview
simple_title:         "חישוב תאריך בעתיד או בעבר"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

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
