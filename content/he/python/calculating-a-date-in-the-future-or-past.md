---
title:                "חישוב תאריך בעתיד או בעבר"
html_title:           "Python: חישוב תאריך בעתיד או בעבר"
simple_title:         "חישוב תאריך בעתיד או בעבר"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## מה ולמה?

חישוב תאריך בעתיד או בעבר מתייחס לפעולה של הוספת או חיסור ימים, חודשים, או שנים מתאריך מסוים. מתכנתים עלולים להשתמש בכך לצורכי לוגיסטיקה, השוואת מידע, תזמון אירועים ועוד.

## איך לעשות את זה?

```Python
from datetime import datetime, timedelta

# חישוב תאריך בעתיד
future_date = datetime.now() + timedelta(days=30)
print(future_date)

# חישוב תאריך בעבר
past_date = datetime.now() - timedelta(days=30)
print(past_date)
```
## צלילה עמוקה

ראשית, המושגים של חישוב תאריך בעתיד או בעבר נמשו מעניינים מסחריים ישנים, כמו שכירות, תשלומי לווין, ומלחמות. במהלך השנים אנו מצאנו שימושים נוספים במערכות מחשב. 
אלטרנטיבות לפתרון ה-Python שלנו יכול להיות בשפות אחרות כמו JavaScript או Java, שיש להן הפונקציונליות המשולבת שלהן.
בפיתוח, חשוב לזכור שהפונקציות `timedelta` ו`datetime.now()` מחזירות תאריך ושעה, לא רק תאריך. אם רק התאריך נחוץ, ניתן להשתמש ב-future_date.date() או past_date.date().

## ראה גם

- Python Documentation: datetime — Basic date and time types: [https://docs.python.org/3/library/datetime.html](https://docs.python.org/3/library/datetime.html)
- Python datetime to string without microsecond component: [https://stackoverflow.com/questions/7999935/python-datetime-to-string-without-microsecond-component](https://stackoverflow.com/questions/7999935/python-datetime-to-string-without-microsecond-component)
- Useful Python date( ) Examples: [https://www.w3schools.com/python/python_datetime.asp](https://www.w3schools.com/python/python_datetime.asp)