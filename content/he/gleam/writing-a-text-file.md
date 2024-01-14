---
title:    "Gleam: כתיבת קובץ טקסט"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## למה

לומר שכידון קלט מטרה בה תרצה לכתוב את הקובץ הנוכחי, יכול להיות יעיל כאשר מטרתך היא לכתוב קוד בפורמט מסוים או כאשר ביקורת על קוד קיימת נדרשת.

סקריפטים פרומטריים דורשים לעסוק בנתוני מסוימים, כך שכתיבת קבצי טקסט יכולה לסייע לך להתמודד עם דרישות אלה ולשפר את הביצועים של הקוד שלך.

## איך לעשות

כדי לכתוב קובץ טקסט בשפת גלים, תחליף להשתמש בפונקציות בשם `gleam.core.writer` ו `gleam.io.file`. למשל, כך נראה קוד Gleam פשוט שמקבל טקסט כקלט ויוצר קובץ טקסט עם התוכן הנתון:

```
mymodule:file_writer_example() =
  let
    file = io.file.create("my_text_file.txt")
    writer = gleam.core.writer.with_file(file)
    text = "Hello world!"
    _ = writer.write(text)
    _ = writer.close()
  in
    file
```

הפונקציה הזו משתמשת בפונקציות שנויות כדי ליצור את הקובץ הנתון ולכתוב אליו את הטקסט המסוים. כאשר הפונקציה תתבצע, יוצר קובץ טקסט חדש בשם `my_text_file.txt` וממלא אותו בתוכן המוגדר במשתנה `text`.

### דוגמאות נוספות

אם בקובץ הטקסט יש תווים בלי משמעות, ניתן להשתמש בפונקציה `gleam.core.writer.with_encoding` כדי להגדיר את קידוד התווים המתאים:

```
mymodule:write_file_with_encoding() =
  let
    file = io.file.create("my_text_file.txt")
    writer = gleam.core.writer.with_encoding(file, "UTF-8")
    text = "Привет, мир!" // (שפה: רוסית)
    _ = writer.write(text)
    _ = writer.close()
  in
    file
```

## לחגור לעבר העומק

כמו כל דבר אחר בשפת גלים, כתיבת קבצי טקסט מאפשרת גישה לעבודה עם ספרי