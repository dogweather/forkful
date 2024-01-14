---
title:                "Rust: יצירת קובץ זמני"
simple_title:         "יצירת קובץ זמני"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## למה

יצירת קובץ זמני יכול להיות כלי מאוד שימושי בתהליך פיתוח של תוכנות בשפת Rust. קבצים זמניים מאפשרים לנו ליצור מדגם בזמנו, למלא אותו במידע ולבצע בדיקות בצורה מהירה ויעילה.

## איך ליצור קובץ זמני ב־Rust

כדי ליצור קובץ זמני ב־Rust ניתן להשתמש בפונקציות המובנות בשפה כמו `std::fs::File::create` ולספק להן את הנתיב של הקובץ המיוצר. הנה דוגמה ליצירת קובץ זמני בשם "temp.txt":

```Rust
use std::fs::File;
use std::io::prelude::*;

fn main() {
    let mut file = File::create("temp.txt").expect("נכשל ליצור קובץ זמני");
    file.write_all(b"שלום עולם!").expect("נכשל לכתוב לקובץ זמני");
}
```

פקודת ה־`expect` משמשת לטובה כדי לטפל בתקלות פוטנציאליות, כך שנוכל לראות הודעה תיאורטית אם משהו לא עובד כמצופה. הפקודות הבאות מדגימות כיצד ניתן לכתוב לקובץ זמני ולקרוא ממנו:

```Rust
use std::fs::File;
use std::io::prelude::*;

fn main() {
    // כתיבה לקובץ זמני
    let mut file = File::create("temp.txt").expect("נכשל ליצור קובץ זמני");
    file.write_all(b"שלום עולם!").expect("נכשל לכתוב לקובץ זמני");

    // קריאה מקובץ זמני
    let mut file = File::open("temp.txt").expect("נכשל לפתוח קובץ זמני");
    let mut contents = String::new();
    file.read_to_string(&mut contents).expect("נכשל לקרוא מקובץ זמני");
    println!("תוכן הקובץ הוא:\n{}", contents);
}

```

תוצאות הריצה של הקוד הנ"ל יהיו:

```
תוכן הקובץ הוא:
שלום עולם!
```

כאן משתמשים בפונקציות של הטייפ `String` כדי לעבוד עם התוכן של הקובץ, אך ניתן להשתמש