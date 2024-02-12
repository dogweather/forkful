---
title:                "קבלת התאריך הנוכחי"
aliases:
- he/python/getting-the-current-date.md
date:                  2024-02-03T19:10:52.679751-07:00
model:                 gpt-4-0125-preview
simple_title:         "קבלת התאריך הנוכחי"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

איסוף התאריך הנוכחי ב-Python הוא פעולה בסיסית עבור מגוון יישומים, כמו תיעוד, ניתוח נתונים, וקבלת החלטות בהתבסס על זמן. מדובר באחזור התאריך הנוכחי של המערכת, שהוא קריטי למשימות התלויות בהקשר הזמני.

## איך לעשות את זה:

**באמצעות הספריה הסטנדרטית `datetime`:**

המודול `datetime` בספריה הסטנדרטית של Python מספק כיתות לניהול תאריכים ושעות. כדי לקבל את התאריך הנוכחי, ניתן להשתמש במתודה `date.today()`.

```python
from datetime import date

today = date.today()
print(today)  # פלט: YYYY-MM-DD (לדוגמא, 2023-04-05)
```

**עיצוב זמן:**

אם נדרש התאריך הנוכחי בפורמט אחר, המתודה `strftime` מאפשרת לך לציין עיצוב תאריך מותאם אישית:

```python
from datetime import date

today = date.today()
formatted_date = today.strftime('%B %d, %Y')  # פורמט לדוגמא: "April 05, 2023"
print(formatted_date)
```

**השימוש ב-`pendulum` למגוון גדול יותר (ספרייה חיצונית פופולרית):**

`Pendulum` היא ספרייה חיצונית המציעה גישה אינטואיטיבית יותר לעבודה עם תאריכים ושעות ב-Python. היא מרחיבה את הפונקציונליות הסטנדרטית של datetime ומפשטת ניהול אזורי זמן, בין השאר.

ראשית, וודא שהתקנת את `pendulum` דרך pip:

```shell
pip install pendulum
```

לאחר מכן, כדי לקבל את התאריך הנוכחי:

```python
import pendulum

today = pendulum.now().date()
print(today)  # פלט: YYYY-MM-DD (לדוגמא, 2023-04-05)
```

עם `pendulum`, העיצוב פשוט ודומה לגישת `strftime`:

```python
import pendulum

today = pendulum.now()
formatted_date = today.to_formatted_date_string()  # פורמט ברירת מחדל: "Apr 5, 2023"
print(formatted_date)
```
