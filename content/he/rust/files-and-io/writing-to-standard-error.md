---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:35:01.510778-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: Rust \u05DE\u05E1\
  \u05E4\u05E7\u05EA \u05D3\u05E8\u05DA \u05D9\u05E9\u05D9\u05E8\u05D4 \u05DC\u05DB\
  \u05EA\u05D5\u05D1 \u05DC-stderr \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05D4\
  \u05DE\u05E7\u05E8\u05D5 `eprintln!`, \u05D1\u05D3\u05D5\u05DE\u05D4 \u05DC\u05D0\
  \u05D5\u05E4\u05DF \u05E9\u05D1\u05D5 `println!` \u05DE\u05E9\u05DE\u05E9 \u05DC\
  -stdout. \u05D4\u05E0\u05D4 \u05D3\u05D5\u05D2\u05DE\u05D4 \u05D1\u05E1\u05D9\u05E1\
  \u05D9\u05EA."
lastmod: '2024-03-13T22:44:39.014611-06:00'
model: gpt-4-0125-preview
summary: "Rust \u05DE\u05E1\u05E4\u05E7\u05EA \u05D3\u05E8\u05DA \u05D9\u05E9\u05D9\
  \u05E8\u05D4 \u05DC\u05DB\u05EA\u05D5\u05D1 \u05DC-stderr \u05D1\u05D0\u05DE\u05E6\
  \u05E2\u05D5\u05EA \u05D4\u05DE\u05E7\u05E8\u05D5 `eprintln!`, \u05D1\u05D3\u05D5\
  \u05DE\u05D4 \u05DC\u05D0\u05D5\u05E4\u05DF \u05E9\u05D1\u05D5 `println!` \u05DE\
  \u05E9\u05DE\u05E9 \u05DC-stdout."
title: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\u05E9\u05D2\u05D9\u05D0\u05D4 \u05D4\
  \u05EA\u05E7\u05E0\u05D9\u05EA"
weight: 25
---

## איך לעשות:
Rust מספקת דרך ישירה לכתוב ל-stderr באמצעות המקרו `eprintln!`, בדומה לאופן שבו `println!` משמש ל-stdout. הנה דוגמה בסיסית:

```rust
fn main() {
    eprintln!("זוהי הודעת שגיאה!");
}
```

פלט לדוגמה (לשגיאה סטנדרטית):
```
זוהי הודעת שגיאה!
```

ליותר שליטה על הודעות השגיאה, כמו כאשר אתה רוצה לעצב טקסט או לטפל בתוצאות קלט/פלט, ניתן להשתמש בפונקציה `stderr` מהמודול `std::io`. שיטה זו מספקת ידית לזרם ה-globel stderr, שניתן לכתוב אליו באמצעות שיטות כמו `write_all` או `writeln` מה- trait `Write`:

```rust
use std::io::{self, Write};

fn main() {
    let stderr = io::stderr();
    let mut handle = stderr.lock();
    
    writeln!(handle, "הודעת שגיאה מעוצבת: {}", 404).expect("נכשל בכתיבה ל-stderr");
}
```

פלט לדוגמה (לשגיאה סטנדרטית):
```
הודעת שגיאה מעוצבת: 404
```

אם אתה פועל בסביבות או אפליקציות שבהן אתה מסתמך על ספריות לתיעוד שגיאות או לטיפול בהן, ספריות כמו `log` ו-`env_logger` פופולריות. למרות שהן משמשות יותר לצרכי תיעוד, הן ניתנות להגדרה ויכולות להפנות רמות יומן שגיאות ל-stderr. להלן דוגמה פשוטה לשימוש ב-`log` וב-`env_logger`:

ראשית, הוסף את התלותות ל-`Cargo.toml` שלך:
```toml
[dependencies]
log = "0.4"
env_logger = "0.9"
```

לאחר מכן, התקן והשתמש בתיעוד באפליקציה שלך:
```rust
fn main() {
    env_logger::init();
    log::error!("זוהי הודעת שגיאה שנרשמה ל-stderr");
}
```

הרצת תוכנית זו (לאחר הגדרת `env_logger` עם משתנה סביבה מתאים, לדוגמה, `RUST_LOG=error`) תוציא את הודעת השגיאה ל-stderr, בעזרת התשתית של תיעוד השגיאות.

```plaintext
ERROR: זוהי הודעת שגיאה שנרשמה ל-stderr
```
