---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:42.668431-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05E4\u05D9\u05D9\
  \u05EA\u05D5\u05DF \u05DE\u05E1\u05E4\u05E7\u05EA \u05D3\u05E8\u05DB\u05D9\u05DD\
  \ \u05D9\u05D9\u05D7\u05D5\u05D3\u05D9\u05D5\u05EA \u05DC\u05D1\u05D3\u05D5\u05E7\
  \ \u05D0\u05DD \u05E1\u05E4\u05E8\u05D9\u05D9\u05D4 \u05E7\u05D9\u05D9\u05DE\u05EA\
  \ \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05D4\u05DE\u05D5\u05D3\u05D5\u05DC\
  \u05D9\u05DD `os` \u05D5-`pathlib`. \u05D4\u05E0\u05D4 \u05D3\u05D5\u05D2\u05DE\u05D0\
  \u05D5\u05EA \u05E2\u05D1\u05D5\u05E8 \u05E9\u05E0\u05D9\u05D4\u05DD: #."
lastmod: '2024-03-13T22:44:38.659436-06:00'
model: gpt-4-0125-preview
summary: "\u05E4\u05D9\u05D9\u05EA\u05D5\u05DF \u05DE\u05E1\u05E4\u05E7\u05EA \u05D3\
  \u05E8\u05DB\u05D9\u05DD \u05D9\u05D9\u05D7\u05D5\u05D3\u05D9\u05D5\u05EA \u05DC\
  \u05D1\u05D3\u05D5\u05E7 \u05D0\u05DD \u05E1\u05E4\u05E8\u05D9\u05D9\u05D4 \u05E7\
  \u05D9\u05D9\u05DE\u05EA \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05D4\u05DE\
  \u05D5\u05D3\u05D5\u05DC\u05D9\u05DD `os` \u05D5-`pathlib`."
title: "\u05D1\u05D3\u05D9\u05E7\u05D4 \u05D0\u05DD \u05E1\u05E4\u05E8\u05D9\u05D9\
  \u05D4 \u05E7\u05D9\u05D9\u05DE\u05EA"
weight: 20
---

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
