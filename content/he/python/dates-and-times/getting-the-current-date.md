---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:52.679751-07:00
description: "\u05D0\u05D9\u05E1\u05D5\u05E3 \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA\
  \ \u05D4\u05E0\u05D5\u05DB\u05D7\u05D9 \u05D1-Python \u05D4\u05D5\u05D0 \u05E4\u05E2\
  \u05D5\u05DC\u05D4 \u05D1\u05E1\u05D9\u05E1\u05D9\u05EA \u05E2\u05D1\u05D5\u05E8\
  \ \u05DE\u05D2\u05D5\u05D5\u05DF \u05D9\u05D9\u05E9\u05D5\u05DE\u05D9\u05DD, \u05DB\
  \u05DE\u05D5 \u05EA\u05D9\u05E2\u05D5\u05D3, \u05E0\u05D9\u05EA\u05D5\u05D7 \u05E0\
  \u05EA\u05D5\u05E0\u05D9\u05DD, \u05D5\u05E7\u05D1\u05DC\u05EA \u05D4\u05D7\u05DC\
  \u05D8\u05D5\u05EA \u05D1\u05D4\u05EA\u05D1\u05E1\u05E1 \u05E2\u05DC \u05D6\u05DE\
  \u05DF. \u05DE\u05D3\u05D5\u05D1\u05E8 \u05D1\u05D0\u05D7\u05D6\u05D5\u05E8 \u05D4\
  \u05EA\u05D0\u05E8\u05D9\u05DA \u05D4\u05E0\u05D5\u05DB\u05D7\u05D9 \u05E9\u05DC\
  \u2026"
lastmod: '2024-03-13T22:44:38.653079-06:00'
model: gpt-4-0125-preview
summary: "\u05D0\u05D9\u05E1\u05D5\u05E3 \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05D4\
  \u05E0\u05D5\u05DB\u05D7\u05D9 \u05D1-Python \u05D4\u05D5\u05D0 \u05E4\u05E2\u05D5\
  \u05DC\u05D4 \u05D1\u05E1\u05D9\u05E1\u05D9\u05EA \u05E2\u05D1\u05D5\u05E8 \u05DE\
  \u05D2\u05D5\u05D5\u05DF \u05D9\u05D9\u05E9\u05D5\u05DE\u05D9\u05DD, \u05DB\u05DE\
  \u05D5 \u05EA\u05D9\u05E2\u05D5\u05D3, \u05E0\u05D9\u05EA\u05D5\u05D7 \u05E0\u05EA\
  \u05D5\u05E0\u05D9\u05DD, \u05D5\u05E7\u05D1\u05DC\u05EA \u05D4\u05D7\u05DC\u05D8\
  \u05D5\u05EA \u05D1\u05D4\u05EA\u05D1\u05E1\u05E1 \u05E2\u05DC \u05D6\u05DE\u05DF\
  ."
title: "\u05E7\u05D1\u05DC\u05EA \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05D4\u05E0\
  \u05D5\u05DB\u05D7\u05D9"
weight: 29
---

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
