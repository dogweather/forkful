---
title:                "Rust: כתיבת קובץ טקסט"
simple_title:         "כתיבת קובץ טקסט"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/writing-a-text-file.md"
---

{{< edit_this_page >}}

## מדוע
כתיבת קובץ טקסט היא חלק חשוב בתהליך התכנות וכולם צריכים לדעת כיצד לעשות זאת בשפת Rust. לדוגמה, כתיבת קבצי הגדרות נתונים או קבצי תיאור מצב.

## כיצד לעשות זאת
בהמשך תוכלו למצוא דוגמאות לכתיבת קובץ טקסט בשפת Rust באמצעות השתמשות בקובץ טקסט ריק לפני הכתיבה לתוכו. בנוסף, תוכלו למצוא פלט לאחר שהקוד הופעל, באמצעות השתמשות בקוד `println!`.

```Rust
use std::fs::File;
use std::io::prelude::*;

fn main() {
    // יצירת קובץ טקסט חדש בשם "example.txt"
    let mut file = match File::create("example.txt") {
        Ok(file) => file,
        Err(err) => panic!("שגיאה ביצירת קובץ: {:?}", err),
    };

    // הוספת תוכן לקובץ
    match file.write_all(b"שלום עולם!") {
        Ok(_) => println!("כתיבה לקובץ הצליחה"),
        Err(err) => panic!("שגיאה בכתיבה לקובץ: {:?}", err),
    }
}
```

תוצאה בסוף הקוד:

```
כתיבה לקובץ הצליחה
```

## Deep Dive
בכדי לכתוב קובץ טקסט, נצטרך ליצור משתנה מסוג `File` ולהשתמש בפונקציות של המודול `fs` כדי לייצר אותו. כמו כן, יש לשים לב לסוג המשתנה שנתקל בו כאשר משתמשים בפונקציות כמו `write_all` ולהתאים את המחרוזת לקידוד המתאים.

## ראו גם
- [Rust הינה רעיון נפלא עבור תכנות מבוצע-hybrid ותכניות עם זאת](https://www.rust-lang.org/il)
- [מדריך לתוכנת החשבון CargoבRust](https://doc.rust-lang.org/cargo/)
- [מדריך למודול הסטנדרטי של Rust](https://doc.rust-lang.org/std/index.html)