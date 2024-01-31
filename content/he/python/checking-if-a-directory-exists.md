---
title:                "בדיקה האם תיקייה קיימת"
date:                  2024-01-20T14:58:19.043893-07:00
simple_title:         "בדיקה האם תיקייה קיימת"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## מה ולמה?
בדיקה אם תיקייה קיימת ב-Python זה פשוט שימוש בפונקציות בסיסיות כדי לוודא אם נתיב מסוים הוא תיקייה וקיים במערכת הקבצים. מתכנתים עושים את זה כדי למנוע שגיאות הקשורות לקבצים ולתיקיות ולשמור על זרימה חלקה של התוכנית.

## איך עושים את זה:
```Python
import os

# נתיב לתיקייה שאולי קיימת
directory_path = '/path/to/directory'

# בדיקה אם התיקייה קיימת
if os.path.isdir(directory_path):
    print(f"התיקייה {directory_path} קיימת.")
else:
    print(f"התיקייה {directory_path} לא קיימת.")

# פלט:
# התיקייה /path/to/directory לא קיימת.
```

אם אתם רוצים לבדוק וגם ליצור את התיקייה אם היא לא קיימת:
```Python
# בדיקה ויצירה אם צריך
if not os.path.isdir(directory_path):
    os.makedirs(directory_path)
    print(f"נוצרה התיקייה {directory_path}.")
else:
    print(f"התיקייה {directory_path} כבר קיימת.")

# פלט:
# נוצרה התיקייה /path/to/directory.
```

## עיון מעמיק
מאז היווסדה שפת Python, הבדיקה והעבודה עם מערכת הקבצים הייתה חלק בלתי נפרד ממנה. בגרסאות קודמות, שימוש במודול `os.path` היה שיטת הסטנדרט לבדיקות כאלה. עם שפת Python 3.4 ואילך, הוספה הספרייה `pathlib`, שמאפשרת עבודה עם נתיבי קבצים בצורה יותר אינטואיטיבית ומודרנית:

```Python
from pathlib import Path

# נתיב לתיקייה עם pathlib
directory = Path('/path/to/directory')

# בדיקה אם התיקייה קיימת
if directory.is_dir():
    print(f"התיקייה {directory} קיימת.")
else:
    print(f"התיקייה {directory} לא קיימת.")
```

`pathlib` מציעה ממשק מונחה-עצמים ומאפשרת כתיבה נקייה וברורה יותר. כמו כן, היא מספקת פונקציונליות רבה יותר מאשר `os.path`.

## ראו גם
- התיעוד הרשמי של Python למודול `os.path`: https://docs.python.org/3/library/os.path.html
- התיעוד הרשמי של Python לספרייה `pathlib`: https://docs.python.org/3/library/pathlib.html
- מדריך עמוק יותר לעבודה עם נתיבי קבצים ב-Python: https://realpython.com/working-with-files-in-python/
