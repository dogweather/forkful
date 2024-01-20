---
title:                "השוואה בין שני תאריכים"
html_title:           "Arduino: השוואה בין שני תאריכים"
simple_title:         "השוואה בין שני תאריכים"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## מה ולמה?
השוואה של שני תאריכים היא פעולה על ידה נוכל לראות איזה תאריך קורה לפני שני. תוכניתנים עשויים לבצע זאת לשם מעקב אחר זמן, ניהול מידע ואפשרות להפעיל לוזים או אם למשל נקיים מעין "עלעף"- מפגש שבועי במשחק עם חברים על מנת למצוא את השארה של מספר מרובע מול שבר כזה או אחר, שהיה פעיל בינינו בתחילת האלפיים 

## איך לעשות זאת:
על מנת להשוות שני תאריכים ב-Python, תוכל להשתמש בספריית ה- datetime:
```Python
from datetime import datetime

# נזין שני תאריכים למשתנים
date1 = datetime(2021, 5, 18)
date2 = datetime(2022, 5, 18)

# נשווה את התאריכים
if date1 > date2:
    print("date1 is later")
elif date1 < date2:
    print("date2 is later")
else:
    print("dates are the same")

```
הפלט הצפוי:
```
date2 is later
```

## צלילה עמוקה
השוואה של תאריכים היא אחת הפעולות הייחודיות בתכנות, ויש לך שלל אפשרויות להשתמש. בגירסאות הקודמות של Python, עשוי להיות שלא היית מצוייד בפונקציונאליות מרשימה עשויה להשבות עמדה ולהתמודד עם החומר המתקשה כלכלה. שגרת השוואת הוסיפה רעיון של השוואת תאריכים כמו numpy ו-pandas. אימפלמנטציה תלויה אותנו, נשקול את הדרישות והמערכת ההכרחית שיתמודד עמם שאנו מחזיקים. 

## ראה גם
אם אתה מעוניין ללמוד עוד על השוואת תאריכים, אני ממליץ לקרוא את המקורות הבאים:
1. [תיעוד Python על אודות ה- datetime module](https://docs.python.org/3/library/datetime.html)
2. [StackOverflow: כיצד להשוות שני תאריכים ב-Python](https://stackoverflow.com/questions/46551955/python-how-to-compare-two-dates)
3. [יישום הפעולה ב-pandas](https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.Timestamp.html)