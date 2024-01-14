---
title:                "Gleam: הכתיבה של קובץ טקסט"
simple_title:         "הכתיבה של קובץ טקסט"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

## למה?

למה לכתוב קובץ טקסט? ישנם מספר סיבות לכך. בעזרת כתיבה של קבצי טקסט, אנחנו יכולים לשמור מידע חשוב ולשתף אותו עם אחרים, ליצור תיעוד על קוד שאנחנו כותבים ולייצר פתרונות פשוטים למשימות שנתקל בהן.

## איך לעשות זאת?

כדי לכתוב קובץ טקסט בשפת Gleam, נשתמש בפונקציית הקוד write_file. נמצא את המיקום הרצוי לכתיבת הקובץ ונתחיל להכניס את הטקסט שאנחנו רוצים לכתוב, כך:

```Gleam
import gleam/io

fn main() {
  let text = "זוהי התחלה של קובץ טקסט חדש"
  let path = "text.txt"
  io.write_file(path, text)
}
```

כאשר נריץ את הקוד הזה, הקובץ החדש ייווצר והטקסט שנכתב בתוכו יהיה "זוהי התחלה של קובץ טקסט חדש". ניתן גם להשתמש בפונקציה write_file לכתיבת טקסט לקובץ שכבר קיים.

## לחקור עוד

כדי לחקור עוד על כתיבת קבצי טקסט בשפת Gleam, נוכל לבחון עוד פונקציות חשובות כגון read_file ו-append_to_file. באמצעות פונקציה זו ניתן לכתוב קוד שייצר תוכן מסוים לפי הצורך, לקרוא תוכן מקובץ קיים ולהוסיף אותו לקובץ שאנחנו רוצים.

## ראה גם

למידע נוסף על כתיבת קבצי טקסט בשפת Gleam, ניתן לעיין במקורות הבאים:

- [קורס מקוון על שפת Gleam בעברית](https://hebrew.programming.samossi.com/p/gleam-tutorial)
- [דוקומנטציית שפת Gleam](https://gleam.run/book/introduction.html)
- [גיטהאב של שפת Gleam](https://github.com/gleam-lang/gleam)