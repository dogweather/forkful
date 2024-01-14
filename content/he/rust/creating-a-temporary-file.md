---
title:    "Rust: יצירת קובץ זמני"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## מדוע

יצירה של קובץ זמני בשפת Rust נחשבת לאחד הדרכים המומלצות לניהול מניפולציות עם קבצים ותיק ורב עובדי. זה יכול להיות שימושי במצבים רבים, כגון בנית בוטים, פענוח, ניפוי באגים ועוד.

## איך לעשות זאת

ליצור קובץ זמני בשפת Rust ניתן באמצעות הפעולה `fs::File::create()` והשיטות המתאימים לניהול קבצים. ניתן להשתמש בפנקציה `rand::thread_rng().gen()` כדי ליצור שם רנדומלי לקובץ, ואת השם הזה ניתן להשתמש בו כדי ליצור את הקובץ. לדוגמה:

```Rust
use std::fs::File;
use rand::{thread_rng, Rng};

fn main() {
    let file_name: String = thread_rng().gen_ascii_chars().take(10).collect();
    let mut temp_file = File::create(&file_name).expect("Could not create file");
}
```

ברגע שהפעולה נכשלת תפלט שגיאה למסך. אפשר להשתמש גם בפקודת `remove()` כדי למחוק את הקובץ הזמני כאשר הוא יוכל או לא נדרש יותר.

## Deep Dive

כאשר תיצור קובץ זמני, Rust יצר סמן ויחד עם ה-File Descriptor ניתן לכן גם לנהל את הקובץ באמצעות מתודות אחרות על הקובץ. ניתן לקרוא יותר על ה-File Descriptor באתר הרשמי של Rust.

## ראה גם

- [קבצים ותיקים בשפת Rust](https://doc.rust-lang.org/book/ch12-00-an-io-project.html)
- [נהלנו קבצים בשפת Rust](https://doc.rust-lang.org/book/ch12-01-writing-to-a-file.html)
- [הוראות הפעלה כברירת מחדל בשפת Rust](https://doc.rust-lang.org/std/fs/struct.OpenOptions.html#method.create_new)