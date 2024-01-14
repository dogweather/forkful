---
title:                "Rust: קריאת קובץ טקסט"
simple_title:         "קריאת קובץ טקסט"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## למה:

כתיבת קבצי טקסט היא חלק חשוב מאוד בתהליך התכנות ובלמידת שפת התכנות Rust. קריאת קבצי טקסט מאפשרת לנו לעבוד בצורה יעילה יותר עם נתונים וליצור אפליקציות מתקדמות.

## איך לעשות זאת:

תחילה נצטרך ליצור משתנה מסוג `File` שיכיל את הקובץ שנרצה לקרוא. לדוגמה:

```Rust
let file = File::open("myfile.txt").expect("Failed to open file");
```

כעת ניתן לקרוא את הקובץ באמצעות הפעולה `read_to_string()` ולאחר מכן להדפיס את התוכן בעזרת הפעולה `println!()`:

```Rust
let contents = fs::read_to_string(file).expect("Failed to read file");
println!("The contents of the file are:\n{}", contents);
```

כמו כן, ניתן להשתמש בלולאת `for` על מנת לעבור על כל השורות בקובץ ולהדפיס אותן אחת אחרי השניה:

```Rust
for line in contents.lines() {
    println!("{}", line);
}
```

לאחר מכן ניתן לסגור את הקובץ באמצעות הפעולה `close()`:

```Rust
file.close().expect("Failed to close file");
```

## הצצה מעמיקה:

כדי לקרוא קובץ טקסט בצורה נמוכה יותר, ניתן להשתמש במודול `io` ובפעולת `read_line()`. פעולה זו מאפשרת לנו לקרוא כל שורה בקובץ בנפרד ולעבוד איתן באופן עקבי יותר.

```Rust
let file = fs::File::open("myfile.txt").expect("Failed to open file");
let reader = BufReader::new(file);

for line in reader.lines() {
    if let Ok(line) = line {
        println!("{}", line);
    }
}
```

חשוב לסגור את הקובץ בסוף על מנת למנוע כל מקרי תקיעה או שגיאות אפשריות.

## ראו גם:

- [מדריך לקריאת קובץ טקסט ב-Rust](https://doc.rust-lang.org/std/fs/struct.File.html#method.read_to_string)
- [מדריך למודול io ב-Rust](https://doc.rust-lang.org/std/io/index.html)
- [מדריך לשימוש בתנאי if let ב-Rust](https://doc.rust-lang.org/reference/expressions/if-let-expr.html)