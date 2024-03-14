---
date: 2024-01-20 17:41:40.366887-07:00
description: "\u05D9\u05B0\u05E6\u05B4\u05D9\u05E8\u05B7\u05EA \u05E7\u05B9\u05D1\u05B6\
  \u05E5 \u05D6\u05B0\u05DE\u05B7\u05E0\u05B4\u05BC\u05D9 \u05D4\u05D9\u05D0 \u05D4\
  \u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05D1\u05D5 \u05D0\u05EA\u05D4 \u05D9\u05D5\
  \u05E6\u05E8 \u05E7\u05D5\u05D1\u05E5 \u05E9\u05DE\u05E9\u05DE\u05E9 \u05DC\u05E9\
  \u05DD \u05DE\u05D7\u05E7\u05E8, \u05D1\u05D3\u05D9\u05E7\u05D4 \u05D0\u05D5 \u05D0\
  \u05D7\u05E1\u05D5\u05DF \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D6\u05DE\u05E0\
  \u05D9\u05D9\u05DD. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\
  \u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05DE\u05E0\u05D5\u05E2\
  \ \u05D6\u05D9\u05D4\u05D5\u05DD \u05E9\u05DC \u05DE\u05E8\u05D7\u05D1 \u05D4\u05D3\
  \u05D9\u05E1\u05E7 \u05D1\u05E0\u05EA\u05D5\u05E0\u05D9\u05DD\u2026"
lastmod: '2024-03-13T22:44:39.019694-06:00'
model: gpt-4-1106-preview
summary: "\u05D9\u05B0\u05E6\u05B4\u05D9\u05E8\u05B7\u05EA \u05E7\u05B9\u05D1\u05B6\
  \u05E5 \u05D6\u05B0\u05DE\u05B7\u05E0\u05B4\u05BC\u05D9 \u05D4\u05D9\u05D0 \u05D4\
  \u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05D1\u05D5 \u05D0\u05EA\u05D4 \u05D9\u05D5\
  \u05E6\u05E8 \u05E7\u05D5\u05D1\u05E5 \u05E9\u05DE\u05E9\u05DE\u05E9 \u05DC\u05E9\
  \u05DD \u05DE\u05D7\u05E7\u05E8, \u05D1\u05D3\u05D9\u05E7\u05D4 \u05D0\u05D5 \u05D0\
  \u05D7\u05E1\u05D5\u05DF \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D6\u05DE\u05E0\
  \u05D9\u05D9\u05DD. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\
  \u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05DE\u05E0\u05D5\u05E2\
  \ \u05D6\u05D9\u05D4\u05D5\u05DD \u05E9\u05DC \u05DE\u05E8\u05D7\u05D1 \u05D4\u05D3\
  \u05D9\u05E1\u05E7 \u05D1\u05E0\u05EA\u05D5\u05E0\u05D9\u05DD\u2026"
title: "\u05D9\u05E6\u05D9\u05E8\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D6\u05DE\u05E0\
  \u05D9"
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
