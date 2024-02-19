---
aliases:
- /he/python/converting-a-date-into-a-string/
date: 2024-01-20 17:38:29.800976-07:00
description: "\u05D4\u05DE\u05E8\u05EA \u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05D1\u05E4\u05D9\u05D9\u05EA\u05D5\u05DF \u05D4\
  \u05D9\u05D0 \u05E4\u05E2\u05D5\u05DC\u05D4 \u05E9\u05DE\u05D0\u05E4\u05E9\u05E8\
  \u05EA \u05DC\u05D9\u05D9\u05E6\u05D2 \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD\
  \ \u05D1\u05E4\u05D5\u05E8\u05DE\u05D8 \u05E7\u05E8\u05D9\u05D0 \u05DC\u05D0\u05E0\
  \u05E9\u05D9\u05DD \u05D0\u05D5 \u05DC\u05E9\u05DE\u05D5\u05E8 \u05D0\u05D5\u05EA\
  \u05DD \u05D1\u05D1\u05E1\u05D9\u05E1\u05D9 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD\
  \ \u05D5\u05E7\u05D1\u05E6\u05D9\u05DD. \u05EA\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\
  \u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\
  \u05E9\u05E4\u05E8 \u05D0\u05EA \u05D4\u05E0\u05D2\u05D9\u05E9\u05D5\u05EA\u2026"
lastmod: 2024-02-18 23:08:52.448634
model: gpt-4-1106-preview
summary: "\u05D4\u05DE\u05E8\u05EA \u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA \u05D1\u05E4\u05D9\u05D9\u05EA\u05D5\u05DF \u05D4\u05D9\
  \u05D0 \u05E4\u05E2\u05D5\u05DC\u05D4 \u05E9\u05DE\u05D0\u05E4\u05E9\u05E8\u05EA\
  \ \u05DC\u05D9\u05D9\u05E6\u05D2 \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD \u05D1\
  \u05E4\u05D5\u05E8\u05DE\u05D8 \u05E7\u05E8\u05D9\u05D0 \u05DC\u05D0\u05E0\u05E9\
  \u05D9\u05DD \u05D0\u05D5 \u05DC\u05E9\u05DE\u05D5\u05E8 \u05D0\u05D5\u05EA\u05DD\
  \ \u05D1\u05D1\u05E1\u05D9\u05E1\u05D9 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D5\
  \u05E7\u05D1\u05E6\u05D9\u05DD. \u05EA\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD\
  \ \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05E9\
  \u05E4\u05E8 \u05D0\u05EA \u05D4\u05E0\u05D2\u05D9\u05E9\u05D5\u05EA\u2026"
title: "\u05D4\u05DE\u05E8\u05EA \u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
המרת תאריך למחרוזת בפייתון היא פעולה שמאפשרת לייצג תאריכים בפורמט קריא לאנשים או לשמור אותם בבסיסי נתונים וקבצים. תכניתנים עושים זאת כדי לשפר את הנגישות והחליפיות של הנתונים.

## איך לעשות:
בפייתון, המרת תאריך למחרוזת מתבצעת בעזרת ספריית ה-`datetime`. קוד הדוגמה הבא מראה לך איך:

```Python
from datetime import datetime

# יצירת אובייקט תאריך ושעה נוכחי
current_datetime = datetime.now()

# המרה למחרוזת בפורמט סטנדרטי
date_string = current_datetime.strftime('%Y-%m-%d %H:%M:%S')
print(date_string)  # דוגמה לפלט: 2023-03-15 14:58:47

# המרה למחרוזת בפורמט מותאם אישית
custom_format = current_datetime.strftime('%d/%m/%Y %I:%M %p')
print(custom_format)  # דוגמה לפלט: 15/03/2023 02:58 PM
```

## צלילה לעומק:
המרת תאריכים למחרוזות היא חלק מובנה של רוב שפות התכנות וקריטית ליצירת ממשקי משתמש ודיווחים. בפייתון, הפונקציה `strftime` מאפשרת להגדיר את התבנית של התאריך והשעה. התבניות מורכבות ממחרוזות פורמט מיוחדות שיכולות להיות מותאמות. למרות הנוחות הרבה של הפונקציה הזו, יש גם אלטרנטיבות בספריות חיצוניות כגון `Arrow` ו` Pendulum`, הן מספקות גמישות בעיצוב ופונקציונליות נוספת. יתרה מכך, הבנת המנגנונים של `strftime` חשובה למניעת באגים הנובעים מתבניות לא תקינות ובעיות תרגום תאריכים לפורמטים שונים.

## ראו גם:
- תיעוד רשמי של `datetime` בפייתון:
  https://docs.python.org/3/library/datetime.html
- מדריכים למחרוזות תאריך ושעה בפייתון:
  https://strftime.org/
- ספריית `Arrow` לעבודה נוחה עם תאריכים:
  https://arrow.readthedocs.io/en/latest/
- ספריית `Pendulum` לניהול תאריכים מתקדם:
  https://pendulum.eustace.io/
