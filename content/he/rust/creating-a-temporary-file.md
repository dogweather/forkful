---
title:                "יצירת קובץ זמני"
html_title:           "Rust: יצירת קובץ זמני"
simple_title:         "יצירת קובץ זמני"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

כבר שמעת על קבצים זמניים? אם לא, כנראה שאתה לא משתמש בקוד ראסט המתקדם כמו הצדקת. אחד המבניים החשובים ביותר הוא יצירת קבצים זמניים, ובמאמר הזה אני אסביר לך למה וכיצד.

## למה

יצירת קבצים זמניים היא פעולה חשובה בקוד ראסט כי היא מאפשרת לנו ליצור קבצים שימחקו באופן אוטומטי כאשר הם יהיו לא נחוצים יותר. זה יכול להיות שימושי כאשר אנו צריכים ליצור קבצים זמניים כדי לבצע פעולות מסוימות, כגון העתקת קבצים או עריכת קבצים קיימים.

## כיצד לייצר קובץ זמני באמצעות קוד ראסט

ליצור קובץ זמני פשוט באמצעות קוד ראסט הוא פשוט מאוד. השתמש בפונקציה `tempfile::tempfile()` כדי ליצור אובייקט קובץ זמני.

```rust
use std::{fs::File, io::Write};
use tempfile::tempfile;

fn main() {
    // יצירת קובץ זמני במערכת הקבצים
    let mut file = tempfile().expect("Failed to create temporary file");
    
    // Escrevendo para o arquivo temporário
    file.write_all(b"אני אוהב את קוד ראסט!").expect("Failed to write to temporary file");
    
    // קרא את תוכן הקובץ הזמני
    println!("התוכן של הקובץ הזמני הוא: {}", String::from_utf8_lossy(file.read().expect("Failed to read temporary file")));
}
```

מהדוגמא שלנו, התוכן של הקובץ הזמני יודפס כ "אני אוהב את קוד ראסט!".

ניתן גם ליצור קובץ זמני עם סיומת מסוימת באמצעות פונקציית `tempfile::Builder()`. לדוגמא, אם נרצה ליצור קובץ זמני עם סיומת `.txt`:

```rust
use std::{fs::File, io::Write};
use tempfile::Builder;

fn main() {
    // יצירת קובץ זמני עם סיומת `.txt`