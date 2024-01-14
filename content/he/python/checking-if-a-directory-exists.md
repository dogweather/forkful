---
title:    "Python: בדיקת קיום תיקייה במחשב"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## למה

לוודא אם תיקייה קיימת הוא פעולה חשובה בתכנות פייתון, כי כדי לבצע פעולות כמו קריאה וכתיבה לקבצים או לפתיחת תהליך, נדרשת תיקייה קיימת כבסיס החלקה.

## כיצד לבדוק אם תיקייה קיימת

ניתן לבדוק אם תיקייה קיימת באמצעות פונקציית `os.path.exists()` שמחזירה `True` אם התיקייה קיימת ו-`False` אם לא. לדוגמה:

```Python
import os

# בדיקת תיקייה קיימת
if os.path.exists("my_folder"):
    print("התיקייה קיימת")

# בדיקת תיקייה שאינה קיימת
if os.path.exists("my_other_folder"):
    print("התיקייה קיימת")
else:
    print("התיקייה לא קיימת")
```

פלט:

```
התיקייה קיימת
התיקייה לא קיימת
```

## עיון מעמיק בבדיקת תיקייה קיימת

בנוסף לפונקציית `os.path.exists()` קיימות גם פונקציות נוספות לבדיקה של תיקיות וקבצים, כגון `os.path.isdir()` שבודקת אם מדובר בתיקייה ו-`os.path.isfile()` שבודקת אם מדובר בקובץ. כמו כן, ניתן גם להשתמש במודול `pathlib` המציע קליטה יותר מודרנית ונוחה לבדיקה של תיקיות וקבצים. 

## ראו גם

* [מדריך לשימוש בפונקציית `os.path.exists()`](https://docs.python.org/3/library/os.path.html#os.path.exists)
* [מדריך לשימוש במודול `pathlib`](https://docs.python.org/3/library/pathlib.html)