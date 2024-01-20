---
title:                "בדיקה אם ספרייה קיימת"
html_title:           "Java: בדיקה אם ספרייה קיימת"
simple_title:         "בדיקה אם ספרייה קיימת"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## מה ולמה?
בדיקת קיום ספרייה ב-Python היא פעולה שבה אנו בודקים אם ספרייה מסוימת קיימת על המחשב שלנו. פרוגרמיסטים משתמשים בכך להימנע משגיאות במהלך ריצת הקוד – דבר שיכול לקרות אם הם מנסים לגשת לספריות שאינן קיימות.

## כיצד ל:
נמשל את כך בקוד שיכול לדאוג למניעת שגיאות כאשר משתמשים בספריות:

```Python
import os

def check_directory_exists(directory):
  if os.path.isdir(directory):
    print(f"הספרייה {directory} קיימת.")
  else:
    print(f"הספרייה {directory} לא קיימת.")

# בדיקת ספרייה שקיימת:
check_directory_exists("/home")

# בדיקת ספרייה שלא קיימת:
check_directory_exists("/not_a_real_directory")
```

הפלט יהיה:

```
הספרייה /home קיימת.
הספרייה /not_a_real_directory לא קיימת.
```

## Deep Dive
אחת מהיכולות של Python היא להיות מזרחית או מערבית, ובמסגרת האפשרויות שלה היא מציעה שיטות לבדיקת קיימות של ספריות. זה נכון למרות שהצורך בבדיקה קיים פחות ב-Python מאשר בשפות אחרות שהופקות ממנה. 

ילדיי זווית בדיקה זו היא להשתמש בספריית pathlib שמוגדרת ברוב הדרישות של Python 3:

```Python
from pathlib import Path

def check_directory_exists(directory):
  if Path(directory).is_dir():
    print(f"הספרייה {directory} קיימת.")
  else:
    print(f"הספרייה {directory} לא קיימת.")
```

## ראה גם
[מסמכים רשמיים של os.path](https://docs.python.org/3/library/os.path.html)
[מסמכים רשמיים של pathlib](https://docs.python.org/3/library/pathlib.html)