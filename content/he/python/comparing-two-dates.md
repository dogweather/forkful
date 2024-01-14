---
title:                "Python: השוואת שתי תאריכים"
simple_title:         "השוואת שתי תאריכים"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## למה

למה מתועדים תאריכים: כדי להשוות בין שני תאריכים ולקבל את הבדלים בין כן.

## איך לעשות זאת?

```Python
# משמש כדוגמא
# ייבא את מודול התאריכים מספר 1
import datetime

# הגדרת שני תאריכים כארגומנטים
date1 = datetime.date(2021, 6, 10)
date2 = datetime.date(2021, 6, 15)

# שימוש בפעולות הבנויות לתאריכים והדפסת התוצאה
print("היום הוא:", date1.day) # היום הוא: 10
print("החודש הוא:", date2.month) # החודש הוא: 6
print("השנה הוא:", date1.year) # השנה הוא: 2021
print("הספרה של התאריך בשנה:", date2.strftime("%j")) # הספרה של התאריך בשנה: 166
```

## עומק לעיון

השוואה בין שני תאריכים מתבצעת על ידי חישוב ההפרשים בין התאריכים האינטרנטיים. לכן, חשוב לדעת שהפונקציות המובנות במודול התאריכים מאפשרות לנו להשוות ולבדוק את התאריכים לפי מספרי היום, החודש והשנה.

## ראה גם

- [Documentation for the datetime module](https://docs.python.org/he/3.8/library/datetime.html)
- [Python Dates and Times: A Beginner's Guide](https://realpython.com/python-datetime/)
- [Python Dates Explained (Tutorial)](https://www.datacamp.com/community/tutorials/python-datetime-tutorial)