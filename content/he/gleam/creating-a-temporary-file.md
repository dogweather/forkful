---
title:                "יצירת קובץ זמני"
html_title:           "C#: יצירת קובץ זמני"
simple_title:         "יצירת קובץ זמני"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## מה ולמה?

יצירת קובץ זמני היא פעולה שבה מפותח מיצר קובץ שאמור להישאר לזמן מוגבל בלבד. זה שימושי בעיקר כאשר מפתחים צריכים לשמור על נתונים מאוד גדולים או חששיבים בזמן רץ של יישום, או לשמירת מצב ביניים של פעולה מסוימת.

## איך ל:

באמצעות תכנית Gleam ביכולתנו ליצור, לכתוב ולקרוא קבצים. 
המשוואה באה עם הפונקציה `create_temporary_file`.

```gleam
import gleam/io.{write, create_temporary_file, read_all}

fn main() {
  let path = create_temporary_file()
  let data = "זהו קובץ זמני"
  let _ = write(path, data)
  let file_data = read_all(path)

  io.println(file_data)  // זהו קובץ זמני
}
```
הקוד מעלה מייצר קובץ זמני, כותב אליו מחרוזת, ואז מקריא אותו בחזרה.

## צניחה לעומק

יצירת קבצים זמניים היא עיקרונית באה מעליה של שפת התכנות C, שבה נוצר הפונקציה `tmpfile()`. 

במקרים מסוימים, יתכן ונרצה לשלוט את המקום שבו מיוצרים שלנו קבצים זמניים; במקרה כזה, אנו מבצעים שימוש בפונקציה `create_temporary_file_in`.

```gleam
import gleam/io.{write, create_temporary_file_in, read_all}

fn main() {
  let path = create_temporary_file_in("/my/dir")
  let data = "זהו קובץ זמני"
  let _ = write(path, data)
  let file_data = read_all(path)

  io.println(file_data)  // זהו קובץ זמני
}
```
שימו לב שבמקום החדש שהגדרנו, אנחנו יוצרים קובץ זמני, כותבים לתוכו, ואז מקריאים אותו.

## ראה גם

ניתן למצוא מידע נוסף בנושאים חשובים במרגעי ביקורת:
* [חיבורי קבצים ומנהלי המשאבים של Gleam](https://gleam.run/book/tour/file-io.html)
* [שימוש בקבצים בשפת התכנות C](https://en.wikipedia.org/wiki/C_file_input/output) 
* [פונקציות מערכת הקבצים של Erlang](http://erlang.org/doc/man/file.html)