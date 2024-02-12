---
title:                "כתיבה לשגיאה התקנית"
date:                  2024-02-03T19:34:48.463760-07:00
model:                 gpt-4-0125-preview
simple_title:         "כתיבה לשגיאה התקנית"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
