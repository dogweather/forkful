---
aliases:
- /he/python/writing-to-standard-error/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:48.463760-07:00
description: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\u05E9\u05D2\u05D9\u05D0\u05D4\
  \ \u05E1\u05D8\u05E0\u05D3\u05E8\u05D8\u05D9\u05EA \u05D1\u05E4\u05D9\u05D9\u05EA\
  \u05D5\u05DF \u05DE\u05D3\u05D5\u05D1\u05E8\u05EA \u05E2\u05DC \u05D4\u05E4\u05E0\
  \u05D9\u05D9\u05EA \u05D4\u05D5\u05D3\u05E2\u05D5\u05EA \u05D4\u05E9\u05D2\u05D9\
  \u05D0\u05D4 \u05D0\u05D5 \u05D0\u05D1\u05D7\u05D5\u05E0\u05D9\u05DD \u05E9\u05DC\
  \ \u05D4\u05EA\u05D5\u05DB\u05E0\u05D9\u05EA \u05DC\u05D6\u05E8\u05DD \u05D4\u05E9\
  \u05D2\u05D9\u05D0\u05D5\u05EA (`stderr`), \u05D1\u05E0\u05E4\u05E8\u05D3 \u05DE\
  \u05D4\u05E4\u05DC\u05D8 \u05D4\u05E1\u05D8\u05E0\u05D3\u05E8\u05D8\u05D9 (`stdout`).\
  \ \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD\u2026"
lastmod: 2024-02-18 23:08:52.453929
model: gpt-4-0125-preview
summary: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\u05E9\u05D2\u05D9\u05D0\u05D4 \u05E1\
  \u05D8\u05E0\u05D3\u05E8\u05D8\u05D9\u05EA \u05D1\u05E4\u05D9\u05D9\u05EA\u05D5\u05DF\
  \ \u05DE\u05D3\u05D5\u05D1\u05E8\u05EA \u05E2\u05DC \u05D4\u05E4\u05E0\u05D9\u05D9\
  \u05EA \u05D4\u05D5\u05D3\u05E2\u05D5\u05EA \u05D4\u05E9\u05D2\u05D9\u05D0\u05D4\
  \ \u05D0\u05D5 \u05D0\u05D1\u05D7\u05D5\u05E0\u05D9\u05DD \u05E9\u05DC \u05D4\u05EA\
  \u05D5\u05DB\u05E0\u05D9\u05EA \u05DC\u05D6\u05E8\u05DD \u05D4\u05E9\u05D2\u05D9\
  \u05D0\u05D5\u05EA (`stderr`), \u05D1\u05E0\u05E4\u05E8\u05D3 \u05DE\u05D4\u05E4\
  \u05DC\u05D8 \u05D4\u05E1\u05D8\u05E0\u05D3\u05E8\u05D8\u05D9 (`stdout`). \u05DE\
  \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD\u2026"
title: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\u05E9\u05D2\u05D9\u05D0\u05D4 \u05D4\
  \u05EA\u05E7\u05E0\u05D9\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבה לשגיאה סטנדרטית בפייתון מדוברת על הפניית הודעות השגיאה או אבחונים של התוכנית לזרם השגיאות (`stderr`), בנפרד מהפלט הסטנדרטי (`stdout`). מתכנתים עושים זאת כדי להבדיל בין פלטי התוכנית הרגילים לבין הודעות השגיאה, מה שמקל על דיבאג וניתוח לוגים.

## איך לעשות:
### באמצעות `sys.stderr`
המודול הפנימי `sys` של פייתון מאפשר כתיבה מפורשת ל-`stderr`. גישה זו פשוטה להודעות שגיאה או אבחונים פשוטים.

```python
import sys

sys.stderr.write('Error: Something went wrong.\n')
```
דוגמת פלט (ל-stderr):
```
Error: Something went wrong.
```

### באמצעות פונקציית `print`
פונקציית ה`print` של פייתון יכולה להפנות את הפלט שלה ל-`stderr` על ידי ציון הפרמטר `file`. שיטה זו שימושית לניצול הנוחות של `print` בזמן טיפול בהודעות שגיאה.
```python
from sys import stderr

print('Error: Failure in module.', file=stderr)
```
דוגמת פלט (ל-stderr):
```
Error: Failure in module.
```

### באמצעות המודול `logging`
לפתרון יותר מקיף, מודול ה`logging` של פייתון יכול להכווין הודעות ל-`stderr` ועוד הרבה יותר, כמו כתיבה לקובץ או התאמה אישית של פורמט ההודעה. שיטה זו הכי טובה ליישומים שדורשים רמות שונות של לוגינג, עיצוב הודעות, או יעדים.
```python
import logging

logging.basicConfig(level=logging.WARNING)
logger = logging.getLogger(__name__)

logger.error('Error: Database connection failed.')
```
דוגמת פלט (ל-stderr):
```
ERROR:__main__:Error: Database connection failed.
```

### ספריות צד שלישי: `loguru`
`loguru` היא ספריית צד שלישי פופולרית שמפשטת את הלוגינג ביישומי פייתון. היא מכווינת אוטומטית שגיאות ל-`stderr`, בין היתר.

לשימוש ב-`loguru`, ראשית התקן אותה דרך pip:
```shell
pip install loguru
```

לאחר מכן, הטמע אותה בסקריפט הפייתון כך:
```python
from loguru import logger

logger.error('Error: Failed to open file.')
```
דוגמת פלט (ל-stderr):
```
2023-04-05 12:00:00.000 | ERROR    | __main__:<module>:6 - Error: Failed to open file.
```
