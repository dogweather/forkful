---
title:                "יצירת קובץ זמני"
html_title:           "Python: יצירת קובץ זמני"
simple_title:         "יצירת קובץ זמני"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## למה

למה ליצור קובץ זמני על ידי קוד Python?

יצירת קובץ זמני יכול להיות מועילה כאשר אנו צריכים לשמור נתונים לזמן קצר בלבד, לדוגמה בעת עבודה עם קבצי מחשב שכבר קיימים או כשאנו רוצים לשמור גיבוי זמני של נתונים.

## איך לעשות זאת

באמצעות פונקציית `tempfile` בספריית הסטנדרטית של Python, יש אפשרות ליצור קובץ זמני בקוד שלנו. ניתן להשתמש בפונקציות כגון `tempfile.NamedTemporaryFile()` או `tempfile.TemporaryFile()` ליצירת קובץ זמני ולבצע פעולות כגון כתיבה וקריאה ממנו. הנה דוגמא של יצירת קובץ זמני וקריאה ממנו:

```Python
import tempfile

# יצירת קובץ זמני עם שם תיקייה וסיומת של TXT
with tempfile.NamedTemporaryFile(suffix=".txt") as temp:
    # כתיבת נתונים לקובץ זמני
    temp.write(b'Hello, Hebrew readers!')
    # קריאת נתונים מהקובץ זמני והדפסתם
    temp.seek(0)
    print(temp.read().decode())
```

הפלט של הדוגמא הנ"ל יהיה:

```
Hello, Hebrew readers!
```

פונקציות כמו `NamedTemporaryFile()` מאפשרות לנו ליצור קובץ זמני עם שם ותיקייה מותאמים אישית ובעלי הרשאות יחודיות. ניתן גם למחוק את הקובץ זמני בקידומת התוכניות דרך `tempfile` כך שהוא לא יישמר לאחר התוכנית תסתיים.

## חקירה מהוקצעת

לאחר יצירת קובץ זמני, ניתן לעיין בפרטים המלאים של הקובץ באמצעות פונקציות כמו `tempfile` ולמצוא את המיקום של הקובץ במחשב. ניתן גם לשנות את המיקום של הקובץ זמני כדי לסנכרן עם ת