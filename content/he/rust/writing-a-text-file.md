---
title:                "כתיבה לקובץ טקסט"
date:                  2024-01-19
html_title:           "Bash: כתיבה לקובץ טקסט"
simple_title:         "כתיבה לקובץ טקסט"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/writing-a-text-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבת קובץ טקסט זה שמירת טקסט בקובץ במערכת הקבצים. תוכניתנים עושים זאת כדי לשמור נתונים, לוגים, ושיתוף מידע עם תוכנות אחרות או משתמשים.

## איך לעשות:
```Rust
use std::fs::File;
use std::io::Write;

fn main() {
    let mut file = File::create("example.txt").expect("Cannot create file");
    file.write_all("שלום, עולם!".as_bytes())
        .expect("Cannot write to file");
}
```
פלט דוגמה: יצירת קובץ example.txt עם הטקסט "שלום, עולם!".

## נפנוף במים
בעבר, כתיבה לקובץ דרשה התעסקות עם תכנות נמוך-רמה יותר. ישנה מגוון של חבילות ב-Rust לכתיבת קבצים; פופולרית נוספת היא `serde` לשריאליזציה ודישריאליזציה. Rust משתמש ב-ownerhip ו-borrowing לניהול זיכרון בזמן ריצה כך שלא תהיינה דליפות זיכרון.

## ראה גם:
- [The Rust Programming Language – File I/O](https://doc.rust-lang.org/rust-by-example/std_misc/file.html)
- [std::fs Module Documentation](https://doc.rust-lang.org/std/fs/index.html)
- [serde: Serialization/Deserialization Crate](https://serde.rs/)
