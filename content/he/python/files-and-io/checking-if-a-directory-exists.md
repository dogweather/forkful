---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:42.668431-07:00
description: "\u05D1\u05D3\u05D9\u05E7\u05D4 \u05D0\u05DD \u05E1\u05E4\u05E8\u05D9\
  \u05D9\u05D4 \u05E7\u05D9\u05D9\u05DE\u05EA \u05D1\u05E4\u05D9\u05D9\u05EA\u05D5\
  \u05DF \u05D4\u05D9\u05D0 \u05D1\u05D3\u05D9\u05E7\u05D4 \u05E9\u05DC \u05E7\u05D9\
  \u05D5\u05DE\u05D4 \u05E9\u05DC \u05EA\u05D9\u05E7\u05D9\u05D9\u05D4 \u05D1\u05DE\
  \u05E2\u05E8\u05DB\u05EA \u05D4\u05E7\u05D1\u05E6\u05D9\u05DD \u05DC\u05E4\u05E0\
  \u05D9 \u05D1\u05D9\u05E6\u05D5\u05E2 \u05E4\u05E2\u05D5\u05DC\u05D5\u05EA \u05DB\
  \u05DE\u05D5 \u05E7\u05E8\u05D9\u05D0\u05D4 \u05D0\u05D5 \u05DB\u05EA\u05D9\u05D1\
  \u05D4 \u05E9\u05DC \u05E7\u05D1\u05E6\u05D9\u05DD. \u05DE\u05EA\u05DB\u05E0\u05EA\
  \u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9\
  \ \u05DC\u05DE\u05E0\u05D5\u05E2 \u05E9\u05D2\u05D9\u05D0\u05D5\u05EA\u2026"
lastmod: '2024-03-11T00:14:12.075769-06:00'
model: gpt-4-0125-preview
summary: "\u05D1\u05D3\u05D9\u05E7\u05D4 \u05D0\u05DD \u05E1\u05E4\u05E8\u05D9\u05D9\
  \u05D4 \u05E7\u05D9\u05D9\u05DE\u05EA \u05D1\u05E4\u05D9\u05D9\u05EA\u05D5\u05DF\
  \ \u05D4\u05D9\u05D0 \u05D1\u05D3\u05D9\u05E7\u05D4 \u05E9\u05DC \u05E7\u05D9\u05D5\
  \u05DE\u05D4 \u05E9\u05DC \u05EA\u05D9\u05E7\u05D9\u05D9\u05D4 \u05D1\u05DE\u05E2\
  \u05E8\u05DB\u05EA \u05D4\u05E7\u05D1\u05E6\u05D9\u05DD \u05DC\u05E4\u05E0\u05D9\
  \ \u05D1\u05D9\u05E6\u05D5\u05E2 \u05E4\u05E2\u05D5\u05DC\u05D5\u05EA \u05DB\u05DE\
  \u05D5 \u05E7\u05E8\u05D9\u05D0\u05D4 \u05D0\u05D5 \u05DB\u05EA\u05D9\u05D1\u05D4\
  \ \u05E9\u05DC \u05E7\u05D1\u05E6\u05D9\u05DD. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\
  \u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\
  \u05DE\u05E0\u05D5\u05E2 \u05E9\u05D2\u05D9\u05D0\u05D5\u05EA\u2026"
title: "\u05D1\u05D3\u05D9\u05E7\u05D4 \u05D0\u05DD \u05E1\u05E4\u05E8\u05D9\u05D9\
  \u05D4 \u05E7\u05D9\u05D9\u05DE\u05EA"
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
