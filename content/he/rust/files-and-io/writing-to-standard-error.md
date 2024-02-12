---
title:                "כתיבה לשגיאה התקנית"
aliases: - /he/rust/writing-to-standard-error.md
date:                  2024-02-03T19:35:01.510778-07:00
model:                 gpt-4-0125-preview
simple_title:         "כתיבה לשגיאה התקנית"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
כתיבה לשגיאה סטנדרטית (stderr) ב-Rust מדברת על הפניית הודעות שגיאה ואבחונים לקונסול בנפרד מהפלט הסטנדרטי (stdout). מתכנתים עושים זאת כדי להבדיל בין פלט תוכנית רגיל לבין הודעות שגיאה, מה שהופך את טיפול בשגיאות לנוח יותר או להפנות אותן ליומנים או קבצים במהלך הביצוע.

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
