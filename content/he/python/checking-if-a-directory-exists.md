---
title:                "בדיקה אם ספרייה קיימת"
date:                  2024-02-03T19:08:42.668431-07:00
model:                 gpt-4-0125-preview
simple_title:         "בדיקה אם ספרייה קיימת"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
בדיקה אם ספרייה קיימת בפייתון היא בדיקה של קיומה של תיקייה במערכת הקבצים לפני ביצוע פעולות כמו קריאה או כתיבה של קבצים. מתכנתים עושים זאת כדי למנוע שגיאות כמו `FileNotFoundError`, מה שמבטיח שהאפליקציה תתנהג באופן אמין ולא תתרסק בעת ניסיון להתערבות עם תיקיות.

## איך לעשות:
פייתון מספקת דרכים ייחודיות לבדוק אם ספרייה קיימת באמצעות המודולים `os` ו-`pathlib`. הנה דוגמאות עבור שניהם:

### באמצעות מודול `os`
```python
import os

# הגדרת נתיב הספרייה
dir_path = "/path/to/directory"

# בדיקה אם הספרייה קיימת
if os.path.isdir(dir_path):
    print(f"הספרייה {dir_path} קיימת.")
else:
    print(f"הספרייה {dir_path} לא קיימת.")
```

### באמצעות מודול `pathlib`
```python
from pathlib import Path

# הגדרת נתיב הספרייה
dir_path = Path("/path/to/directory")

# בדיקה אם הספרייה קיימת
if dir_path.is_dir():
    print(f"הספרייה {dir_path} קיימת.")
else:
    print(f"הספרייה {dir_path} לא קיימת.")
```

### ספריות צד שלישי
למרות שהספרייה הסטנדרטית של פייתון מספיקה לבדוק אם ספרייה קיימת, ספריות כמו `pathlib2` יכולות להיות חלופות לעקביות ברחבי גרסאות פייתון או פונקציונליות נוספת.

***הערה:*** נכון לגרסאות האחרונות של פייתון, `pathlib` מספק בעיקר לרוב הצרכים, דבר שהופך את ספריות צד שלישי לפחות נחוצות למטלה הספציפית הזו.
