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

## מה ולמה?
יצירת קובץ זמני היא תהליך שימושי לכתיבת קוד בפייתון. היא מאפשרת לנו ליצור קובץ שיימחק אוטומטית לאחר שהתהליך מסתיים. תהליך זה מקל עלינו לנהל זיכרון ולמנוע טעיות נוספות בקוד שלנו.

## איך לעשות?
להלן כמה דוגמאות לכיצד ליצור קובץ זמני בפייתון ואת הפלט המשוער שנקבל:

```Python
import tempfile

# דוגמא 1:
with tempfile.TemporaryFile() as tmp:
  tmp.write(b'Hello World!')
  tmp.seek(0)
  print(tmp.read()) # b'Hello World!'

# דוגמא 2:
with tempfile.TemporaryDirectory() as tmp:
  print(tmp) # /var/folders/5q/dj4m_jj12_z0gzqmsrks3k980000gn/T/tmpfj_2_fa1

# דוגמא 3:
with tempfile.NamedTemporaryFile() as tmp:
  tmp.write(b'This is a named temporary file!')
  print(tmp.name) # /var/folders/5q/dj4m_jj12_z0gzqmsrks3k980000gn/T/tmptidl_62z

# דוגמא 4:
with tempfile.SpooledTemporaryFile(max_size=10) as tmp:
  tmp.write(b'Temporary file overflow!')
  print(tmp.spool.max_size) # 10
```

## חפירה עמוקה
יצירת קובץ זמני נחשבת לרעיון מתקדם של פרוגרמינג. הרעיון מופיע ראשונה בשפת תכנות C בשם mkstemp ובהמשך יובא לפייתון כדי לפשט את תהליך יצירת הקבצים הזמניים. אם אין צורך בשמירת נתונים מסוימים בקובץ, ניתן להשתמש בפונקציות חוברות כמו tempfile.TemporaryFile או tempfile.SpooledTemporaryFile כדי למנוע התקלות בזיכרון.

## ראה גם
למידע נוסף על יצירת קבצים זמניים בפייתון ניתן לעיין בתיעוד הרשמי של פייתון או במאמר "Understanding the Temporary File" באתר GeeksforGeeks.