---
title:                "כתיבת קובץ טקסט"
html_title:           "Rust: כתיבת קובץ טקסט"
simple_title:         "כתיבת קובץ טקסט"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/writing-a-text-file.md"
---

{{< edit_this_page >}}

תכנות בשפת ראסט: כתיבת קובץ טקסט מאת: נעם כהן

## מה ולמה?
כתיבת קובץ טקסט היא פעולה שמאפשרת לנו ליצור קובץ שמכיל תוכן טקסטואלי, כגון מסמכי טקסט, קבוצות תווים, וכו'. כתיבת קובץ טקסט היא כלי חשוב לתכנותנים מתוך הצורך ליצור קבצים בתוכניות שלנו או ליבוב תוכן במקום אחר.

## כיצד לכתוב קובץ טקסט בשפת ראסט?
כדי לכתוב קובץ טקסט באמצעות ראסט, נוכל להשתמש בפונקציית "write!" כדי ליצור קובץ חדש ולהכניס לו תוכן. נעבוד עם פורמט טקסטואלי פשוט כדי להדפיס את התוכן שנכתב:

```Rust
use std::fs::File;
use std::io::prelude::*;

fn main() {
    let mut file = File::create("new_file.txt").expect("Failed to create file");
    write!(file, "Hello from Rust!"); // יוצר קובץ טקסט חדש ומכניס אליו את הטקסט

    let mut contents = String::new();
    file.read_to_string(&mut contents).expect("Failed to read file");
    println!("Contents of file: {}", contents); // מדפיס את התוכן שנכתב לקובץ טקסט
}
```

פלט התוכנית יהיה:

```
Contents of file: Hello from Rust!
```

## חפירה עמוקה
כתיבת קובץ טקסט היא פעולה יסודית ונפוצה ביותר בתכנות. היא מאפשרת לנו ליצור קבצים שונים ולכתוב אליהם תוכן בצורה נוחה וקודים להתאמה אישית. ישנן גם אפשרויות נוספות ליצירת קבצים טקסטואליים, כגון יצירת מבני נתונים וכו'.

## ראה גם
- [קובץ טקסט](https://he.wikipedia.org/wiki/%D7%A7%D7%95%D7%91%D7%A5_%D7%98%D7%A7%D7%A1%D7%98)
- [פונקציית "write!" בשפת ראסט](https://doc.rust-lang.org/std/macro.write.html)