---
title:                "Python: יצירת קובץ זמני"
simple_title:         "יצירת קובץ זמני"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## למה

יתרון ראשון של יצירת קובץ זמני הוא שהוא מאפשר לנו ליצור קובץ שהריצה שלו נמשכת רק כל פעם אחת. מה שמונע מאיתנו להשתמש בחלקים אחרים של המחשב שאינם עובדים מספיק טוב.

## איך לעשות

הנה דוגמא פשוטה של שימוש בפייתון בכדי ליצור קובץ זמני, ולאחר מכן לכתוב נתונים ולסגור אותו.

```Python
import tempfile

# יצירת קובץ זמני
temp_file = tempfile.NamedTemporaryFile()

# כתיבת נתונים לקובץ
temp_file.write(b"שלום, עולם")

# סגירת הקובץ
temp_file.close()

# הדפסת נתונים מהקובץ
with open(temp_file.name) as f:
  print(f.read())
```

הפלט של קוד זה יהיה:

```bash
שלום, עולם
```

בקוד זה, אנו משתמשים במודול `tempfile` כדי ליצור קובץ זמני. נקודת התחלה לשמור על פייתון היא ליצור את האובייקט `NamedTemporaryFile`. כאשר אנו פותחים קובץ זמני, יש לו שם ייחודי שניתן לגשת לו על ידי `name`.

לאחר מכן, אנו כותבים נתונים לקובץ באמצעות הפעולה `write`. במקרה זה, אנחנו משתמשים בנתוני מחרוזת שמוצפנת באמצעות `b`.

ולבסוף, אנו סוגרים את הקובץ באמצעות הפעולה `close`. כדי לקרוא את הנתונים מהקובץ, אנו משתמשים ב `open` עם `temp_file.name`, כך שניתן יהיה לגשת לקובץ באופן זמני.

## העמקה מה`tempfile`

המודול `tempfile` מספק מספר אפשרויות נוספות עבור קבצים זמניים. ניתן להשתמש ב `TemporaryDirectory` כדי ליצור תיקיית קבצים זמנית, או `SpooledTemporaryFile` כדי ליצור קובץ זמני עם לכתיבה בתכ