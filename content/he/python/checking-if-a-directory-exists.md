---
title:                "The title should be removed from the translation"
html_title:           "Python: The title should be removed from the translation"
simple_title:         "The title should be removed from the translation"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## למה

הבדיקה אם תיקייה קיימת היא פעולה חשובה בתכנות פייתון שמאפשרת למשתמש לוודא שהתיקייה שהוא מנסה ליצור או לגשת אליה כבר קיימת במחשב שלו.

## איך לבדוק אם תיקייה קיימת בפייתון

```python
# מניפולציות עם מודול ה-os
import os

# הגדרת נתיב לתיקייה שנרצה לבדוק 
path = '/Users/username/Desktop/my_folder'

# בדיקה אם התיקייה קיימת
if os.path.exists(path):
  print("התיקייה קיימת במחשב שלך!")
else:
  print("התיקייה לא קיימת, אנא בדוק שנתיב התיקייה נכון או צור תיקייה חדשה.")
```

פלט:

```
התיקייה קיימת במחשב שלך!
```

## העומק בבדיקת תיקיות בפייתון

בנוסף לשימוש בפקודת `os.path.exists()` ניתן גם להשתמש בפקודות נוספות של מודול ה-os כדי לבדוק אם תיקייה קיימת. למשל, `os.path.isdir()` מכפילה את פקודת השאילתא הסטנדרטית `os.path.exists()` עבור תיקיות בלבד. כמו כן, ניתן להשתמש בפקודת `os.path.isfile()` כדי לבדוק אם קיים קובץ בנתיב הנתון.

## ראו גם

- [מדריך למודול ה-os בפייתון](https://realpython.com/python-pathlib/#understanding-the-os-debugging-tools)
- [פיתוח קובץ ותיקייה בפייתון עם מודול ה-os](https://gabrielsanchez.me/how-to-create-a-directory-in-python/)
- [מדריך לעבודה עם תיקיות וקבצים בפייתון](https://www.pythonforbeginners.com/files/working-with-files-in-python)