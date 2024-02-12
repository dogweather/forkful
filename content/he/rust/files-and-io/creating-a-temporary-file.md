---
title:                "יצירת קובץ זמני"
aliases: - /he/rust/creating-a-temporary-file.md
date:                  2024-01-20T17:41:40.366887-07:00
model:                 gpt-4-1106-preview
simple_title:         "יצירת קובץ זמני"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
יְצִירַת קֹבֶץ זְמַנִּי היא התהליך שבו אתה יוצר קובץ שמשמש לשם מחקר, בדיקה או אחסון נתונים זמניים. מתכנתים עושים זאת כדי למנוע זיהום של מרחב הדיסק בנתונים שאינם נחוצים לטווח הארוך.

## איך לעשות:
הנה דוגמא פשוטה ב-Rust ליצירת קובץ זמני באמצעות הספרייה `tempfile`:

```rust
use tempfile::NamedTempFile;
use std::io::{Write, Read};

fn main() -> std::io::Result<()> {
    let mut temp_file = NamedTempFile::new()?;
    writeln!(temp_file, "היי, זה טקסט זמני!")?;

    let mut content = String::new();
    temp_file.reopen()?.read_to_string(&mut content)?;
    println!("תוכן הקובץ: {}", content);
    
    Ok(())
}
```

פלט לדוגמה:
```
תוכן הקובץ: היי, זה טקסט זמני!
```

## טבילת אש:
ספריית ה-`tempfile` בחסות סטנדרט ה-POSIX ברוב המערכות ההפעלה. קיימות אלטרנטיבות כמו שימוש בספרייה `std::fs` ליצירת קובץ עם פונקציה כמו `File::create`, אבל אז תצטרך לנהל את שם הקובץ ומחיקתו באופן ידני. `tempfile` מקלה עליך את החיים על ידי טיפול אוטומטי בניקוי.

## גם כדאי לראות:
- דוקומנטציה של ספריית `tempfile`: https://docs.rs/tempfile/latest/tempfile/
- חוקי נתיב הקבצים ב-Rust: https://doc.rust-lang.org/std/path/index.html
- מדריך לגבי קבצים וזרם קלט/פלט ב-Rust: https://doc.rust-lang.org/book/ch12-00-an-io-project.html
