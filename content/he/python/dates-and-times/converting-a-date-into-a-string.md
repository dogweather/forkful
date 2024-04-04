---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:37:21.660960-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05E4\u05D9\u05D9\
  \u05EA\u05D5\u05DF \u05DE\u05E7\u05DC \u05E2\u05DC \u05D4\u05DE\u05E8\u05EA \u05EA\
  \u05D0\u05E8\u05D9\u05DB\u05D9\u05DD \u05DC\u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA\
  . \u05D4\u05E9\u05EA\u05DE\u05E9\u05D5 \u05D1\u05E9\u05D9\u05D8\u05EA [`strftime`](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior)\u2026"
lastmod: '2024-04-04T02:03:01.318231-06:00'
model: gpt-4-0125-preview
summary: "\u05E4\u05D9\u05D9\u05EA\u05D5\u05DF \u05DE\u05E7\u05DC \u05E2\u05DC \u05D4\
  \u05DE\u05E8\u05EA \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD \u05DC\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05D5\u05EA."
title: "\u05D4\u05DE\u05E8\u05EA \u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA"
weight: 28
---

## איך לעשות:
פייתון מקל על המרת תאריכים למחרוזות. השתמשו בשיטת [`strftime`](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior) הזמינה באובייקטי [date](https://docs.python.org/3/library/datetime.html#date-objects). כך עושים זאת:

```Python
from datetime import datetime

# קבלת התאריך והשעה הנוכחיים
now = datetime.now()

# המרה למחרוזת בפורמט: חודש יום, שנה
date_string = now.strftime("%B %d, %Y")
print(date_string)  # פלט: March 29, 2023 (או התאריך הנוכחי)

# פורמט: YYYY-MM-DD
iso_date_string = now.strftime("%Y-%m-%d")
print(iso_date_string)  # פלט: 2023-03-29 (או התאריך הנוכחי)
```


### כך אני עושה זאת

כך אני מקבל תאריך בפורמט [ISO 8601](https://www.w3.org/QA/Tips/iso-date) עם מידע על אזור הזמן:

```python
def datestamp() -> str:
    """ 
    התאריך והשעה הנוכחיים עם אזור הזמן בפורמט ISO.
    """
    return datetime.now().astimezone().isoformat()
```

#### דוגמת פלט:

```python
>>> datestamp()
'2024-04-04T01:50:04.169159-06:00'
```



## צלילה עמוקה
היסטורית, המרה של תאריכים למחרוזות הייתה בסיסית בתכנות עקב הצורך לייצג תאריכים בפורמט קריא לאדם.

אלטרנטיבות ל-`strftime` כוללות שימוש בשיטת `isoformat` עבור פורמט ISO 8601, או ספריות צד שלישי כמו `arrow` ו-`dateutil` המציעות אפשרויות ניתוח ועיבוד גמישות יותר.

מבחינה טכנית, `strftime` מסמל "string format time" ויש לו שורשים בתכנות C. `strftime` של פייתון מפרש קודי פורמט כמו `%Y` עבור השנה ו-`%m` עבור החודש, מה שמאפשר גמישות כמעט בלתי מוגבלת.

## ראו גם
להעמקה נוספת בתכונות התאריך והשעה של פייתון:
- מסמכים רשמיים של `datetime` בפייתון: https://docs.python.org/3/library/datetime.html
- למי שמעוניין ברשימה מקיפה של הוראות `strftime`: https://strftime.org/
- לחקור ספריות צד שלישי לתאריך/שעה:
  - Arrow: https://arrow.readthedocs.io/en/latest/
  - python-dateutil: https://dateutil.readthedocs.io/en/stable/
