---
title:    "Python: יצירת קובץ זמני"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# מדוע

יצירת קובץ זמני היא כלי מאוד שימותי בתוך תהליך התכנות שלכם. זה יכול לשמש ליצירת קבצים זמניים לחישובים מסובכים או לשמירת נתונים שמורים במקום כמו זמן ריצת התוכנית.

# כיצד לצור קובץ זמני בפייתון

```Python
import tempfile
# יצירת קובץ זמני באמצעות הפונקציה TemporaryFile
temp_file = tempfile.TemporaryFile()
# כתיבה לקובץ זמני
temp_file.write("זהו קובץ זמני מצויין!")
# קריאה מהקובץ זמני
temp_file.seek(0)
print(temp_file.read())
```
Result:
```
זהו קובץ זמני מצויין!
```

# חקירה מעמיקה

יצירת קובץ זמני היא תהליך שבו תוכנית יצירת קובץ מתבצעת בזמן ריצת הקוד, וכאשר התוכנית סיימה את הריצה - הקובץ מתבטל. זה אומר שהקובץ זמני לא נשמר בזיכרון הקבוע ומבליט את הנתונים המכירים על ידי התוכנית.

# ראה גם
- [מדריך פייתוני על יצירת קבצים זמניים](https://realpython.com/temporary-files-python/)
- [מדריך לניהול קבצים זמניים בפייתון](https://www.geeksforgeeks.org/temporary-files-python/)