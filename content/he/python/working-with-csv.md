---
title:                "עבודה עם CSV"
html_title:           "Python: עבודה עם CSV"
simple_title:         "עבודה עם CSV"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/working-with-csv.md"
---

{{< edit_this_page >}}

## מה ולמה
עבודה עם קבצי CSV היא פעולה נפוצה בעולם התכנות שמאפשרת לנו לקרוא ולכתוב נתונים ממוחשבים בפורמט נוח ופשוט. תוכניתנים משתמשים בקבצי CSV כאחד מהאמצעים הפופולריים ביותר לייצוג נתונים כדי לבצע פעולות שונות על הנתונים.

## כיצד ל
כדי למשוך נתונים מקובץ CSV, אנו משתמשים בג'נק של Python. למשל, כדי לקרוא קובץ CSV ולהדפיס את הנתונים שלו, נכתוב את הקוד הבא:

```python
import pandas as pd

df = pd.read_csv('file.csv')
print(df)
```

כדי לכתוב נתונים לקובץ CSV, נשתמש בפקודת `to_csv()` כמו בדוגמה הבאה:

```python
import pandas as pd

data = [['John', 30],['Jane', 28],['Bob', 25]]
df = pd.DataFrame(data, columns=['Name', 'Age'])
df.to_csv('new_file.csv', index=False)
```

פקודת `index=False` מובילה לכך שלא נוספים מספרי שורות לטבלה היוצאת.

## טפסים מתקדמים
קבצי CSV נמצאים בשימוש כבר מאז שנוצרה הטכנולוגיה של מחשבים ותכנות. טכניקה זו מאפשרת לנו לעבוד עם נתונים בצורה פשוטה ויעילה. כמו כן, ישנן טכנולוגיות אחרות לעבודה עם נתונים, כגון קבצי XML ו-JSON, אך קבצי CSV נותנים לנו גמישות רבה יותר.

כאשר אנו עובדים עם קבצי CSV, חשוב לקחת בחשבון כי ייתכן שהנתונים אינם מסודרים בצורה מושלמת וכי ייתכן שנאלצנו לבצע שינויים כדי לאחזר את הנתונים הרצויים בצורה נכונה.

## ראו גם
- תיעוד רשמי של פייתון על עבודה עם קבצי CSV: https://docs.python.org/3/library/csv.html
- מדריך מקיף על עבודה עם נתונים ב-Python: https://realpython.com/python-data-csv/
- למידע נוסף על ההבדלים בין קבצי CSV, XML ו-JSON: https://www.teradata.com/Resources/Whitepaper/the-difference-between-xml-json-and-csv