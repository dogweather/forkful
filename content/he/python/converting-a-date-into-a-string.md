---
title:                "המרת תאריך למחרוזת"
html_title:           "Bash: המרת תאריך למחרוזת"
simple_title:         "המרת תאריך למחרוזת"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?

המרת תאריך למחרוזת היא תהליך שבו תאריך משוגר בצורה של מחרוזת תווים, למשל "2022-07-22". מתכנתים מבצעים את המרה זו כדי להציג את התאריך בצורה מובנת לאנוש או לשמור תאריכים במאגרי נתונים.

## איך לעשות:

Python מציעה את המודול `datetime` לעבודה עם תאריכים. ניתן להפוך תאריך למחרוזת באמצעות המתודה `strftime()`.

```Python
import datetime

# יצירת אובייקט datetime
d = datetime.date(2022, 7, 22)

# המרת האובייקט datetime למחרוזת
s = d.strftime('%Y-%m-%d')

print(s)  # הדפס '2022-07-22'
```

## צלילה עמוקה:

המתודה `strftime()` הוכנסה ל-Python בגרסה 2.2 כחלק מהמודול `datetime`. הקיצור "strf" מגיע מ-"String Format", בעוד "time" מתייחס לזמן. 

ישנם דרכים נוספות להמיר תאריך למחרוזת, אך `strftime()` היא השיטה הנפוצה ביותר בשל גמישותה ונוחותה. 

אנו משתמשים בתווים מיוחדים, הקרויים קידומות תבנית, כדי לקבוע את הפורמט של התאריך במחרוזת. למשל, '%Y' מייצג את השנה בארבע תווים, ו-'%m' מייצג את החודש בשני תווים.

## ראו גם:

1. מודול `datetime` [תיעוד](https://docs.python.org/3/library/datetime.html)
2. מודול `time` [תיעוד](https://docs.python.org/3/library/time.html)
3. מודול `dateutil` [תיעוד](https://dateutil.readthedocs.io/en/stable/)
4. Python  [תיעוד רשמי](https://docs.python.org/3/)