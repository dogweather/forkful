---
title:                "ביצוע תיקון של תאריך ממחרוזת"
html_title:           "Python: ביצוע תיקון של תאריך ממחרוזת"
simple_title:         "ביצוע תיקון של תאריך ממחרוזת"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

מה זה ולמה?

לפעמים כשאנחנו משתמשים במידע, התאריך מופיע בתוך מחרוזת. פיענוח תאריך ממחרוזת הוא תהליך שבו נמצאת תאריך מסוים בתוך מחרוזת ומפרשים אותו כתאריך ממשי. תהליך זה נדרש כדי לעבוד עם מספר מטלות לוגיות ותוכנות כמו אירוח אינטרנט, ממשקי מנהל תוכנה ויישומי תאריך אחרים.

איך? 

```python
from datetime import datetime
date_string = "01/01/2020"
parsed_date = datetime.strptime(date_string, '%m/%d/%Y')
print(parsed_date)
```

פלט:

```bash
2020-01-01 00:00:00
```

```python
from dateutil.parser import parse
date_string = "January 1, 2020"
parsed_date = parse(date_string)
print(parsed_date)
```

פלט:

```bash
2020-01-01 00:00:00
```

טייפים שונים של מחרוזות תאריך קלות ניתן לפרוס ממחרוזת לתאריך בעזרת פינוי אותו בעזרת אנליזה, כמו מחרוזת שמכילה מונים ואותיות חדשים או תאריכים התאריך מחרוזת עם נגזרת אחרת.

מעומק של המידע

תאריכים מחרוזות יכולים להיות מאוד מגוונים במאגר המידע של מספר אוקליהינים עם.

ראה גם

תאריך קלות מאגרי המידע הנוספים בעין מנסוניות וללהם