---
title:                "עבודה עם קבצי CSV"
date:                  2024-01-19
html_title:           "Arduino: עבודה עם קבצי CSV"
simple_title:         "עבודה עם קבצי CSV"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/working-with-csv.md"
---

{{< edit_this_page >}}

## עקרונות וסיבות
עבודה עם קבצי CSV (ערכים מופרדים בפסיקים) היא טיפול בנתונים טבלאיים בפורמט פשוט ונפוץ. תוכניתנים משתמשים בשיטה זו בגלל נגישותה ותאימותה עם סוגי תוכנה שונים.

## איך לעשות:
קוד לקריאה מ-CSV:
```Python
import csv

# קריאת קובץ CSV
with open('example.csv', newline='') as csvfile:
    reader = csv.reader(csvfile)
    for row in reader:
        print(', '.join(row))
```

קוד לכתיבה ל-CSV:
```Python
import csv

# כתיבת נתונים לקובץ CSV
with open('output.csv', mode='w', newline='') as csvfile:
    writer = csv.writer(csvfile)
    writer.writerow(['name', 'age'])
    writer.writerow(['Alice', 30])
    writer.writerow(['Bob', 25])
```

## פלונטר בעומק:
CSV הופיע בשנות ה-70 כפורמט פשוט לייצוא ויבוא נתונים. ישנם פורמטים אחרים כמו JSON ו-XML אבל CSV נשאר פופולרי בגלל פשטותו. בפייתון, עבודה עם CSV מתבצעת בעזרת המודול `csv` או ספריות חיצוניות כמו `pandas` לטיפול יותר מתקדם.
  
## ראה גם:
- [מדריך קריאה וכתיבה של קבצי CSV ב-Python](https://docs.python.org/3/library/csv.html)
- [ספריית pandas לעבודה עם נתונים טבלאיים](https://pandas.pydata.org/)
- [שיעורים למתחילים בנושא נתונים ב-Python](https://realpython.com/python-csv/)
