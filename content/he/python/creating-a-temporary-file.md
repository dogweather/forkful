---
title:                "יצירת קובץ זמני"
html_title:           "C#: יצירת קובץ זמני"
simple_title:         "יצירת קובץ זמני"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## מה זה ולמה? 

כל קובץ זמני הוא קובץ נתונים שנוצר לשימוש זמני. תכנתים משתמשים בקבצים זמניים כדי לאחסן ולשלוט בזמנית על מספר גדול של נתונים הנכנסים דרך הגרסה או עדכונים בנתונים.

## איך ליצור קובץ זמני?

```Python
import tempfile  # יבוא הספרייה

# יצירת קובץ זמני
temp = tempfile.TemporaryFile()
print(temp)   # הדפסת הקובץ הזמני

# כתיבה לקובץ הזמני
temp.write(b'היי, עולם!')  # כתיבת מסר בקובץ

# קריאת מהקובץ הזמני
temp.seek(0)  # החזרה לתחילת הקובץ
print(temp.read())  # הדפסת תוכן הקובץ
temp.close() # סגירת הקובץ הזמני
```

## מעמקים 

בהקשר ההיסטורי, קבצים זמניים נוצרו לראשונה ממחשבים עם זיכרון מוגבל כדי לשלוט בנתונים גדולים. אפשרויות חלופית לקבצים זמניים כוללות את memory-mapped files והדפסה ישירה לדיסק. ביצירת קובצים זמניים, Python מייצר ברירת מחדל שם קובץ ייחודי במדריך שנבחר.

## ראה גם 
 
- דוקומנטציה אופיציאלית של Python על ספריית tempfile: https://docs.python.org/3/library/tempfile.html
- טוטוריאל אינטראקטיבי על קבצים זמניים ב-Python: https://www.w3schools.com/python/ref_file_read.asp