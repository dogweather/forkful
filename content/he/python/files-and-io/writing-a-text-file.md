---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:48.722258-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: #."
lastmod: '2024-03-13T22:44:38.665952-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8"
weight: 24
---

## איך לעשות:


### באמצעות הפונקציה המובנית `open()`
הפונקציה המובנית `open()` של פייתון היא הדרך הנפוצה ביותר לכתוב לקבצים. הפונקציה מאפשרת לציין את המוד בו הקובץ נפתח - 'w' לכתיבה (דריסה), 'a' להוספה, ו-'w+' לכתיבה+קריאה.

```python
# כתיבה לקובץ חדש או החלפת קובץ קיים
with open('example.txt', 'w') as file:
    file.write("שלום, עולם!\n")

# הוספה לקובץ
with open('example.txt', 'a') as file:
    file.write("מוסיף טקסט נוסף.\n")

# קריאה מהקובץ לאימות
with open('example.txt', 'r') as file:
    print(file.read())
```
**פלט לדוגמא:**
```
שלום, עולם!
מוסיף טקסט נוסף.
```

### באמצעות `pathlib.Path`
לגישה מונחית עצמים יותר, הכיתה `Path` מהמודול `pathlib` מציעה מתודה לכתיבה לקבצים. זו שיטה פופולרית עבור בסיסי קוד של פייתון חדשים יותר.

```python
from pathlib import Path

# כתיבה/החלפת קובץ
Path('example2.txt').write_text("זהו דוגמא 2.\n")

# קריאה מהקובץ לאימות
print(Path('example2.txt').read_text())

# שים לב: `Path.write_text` תמיד דורסת את תוכן הקובץ.
# להוספה, תצטרך לפתוח את הקובץ כפי שהוצג בסעיף הקודם.
```
**פלט לדוגמא:**
```
זהו דוגמא 2.
```

### ספריות צד שלישי
לפעולות קובץ מורכבות יותר, ספריות צד שלישי כמו `pandas` (לקבצי CSV, Excel) יכולות להוות נכס גדול. הנה דוגמא מהירה לכתיבת DataFrame לקובץ CSV באמצעות `pandas`, המדגימה את השימושיות שלה מעבר לקבצי טקסט פשוטים.

```python
# דורשת התקנת pandas: pip install pandas
import pandas as pd

# יצירת DataFrame פשוטה
data = pd.DataFrame({'Column1': [1, 2, 3], 'Column2': ['א', 'ב', 'ג']})

# כתיבת DataFrame לקובץ CSV
data.to_csv('example.csv', index=False)

# קריאה מה-CSV לאימות
print(pd.read_csv('example.csv'))
```
**פלט לדוגמא:**
```
   Column1 Column2
0        1       א
1        2       ב
2        3       ג
```

באמצעות שיטות אלו, מתכנתי פייתון יכולים לנהל פעולות עם קבצים בצורה יעילה, תוך כדי מתן מענה לצרכים פשוטים ומורכבים בטיפול בנתונים.
