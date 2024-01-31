---
title:                "כתיבה לפלט השגיאה הסטנדרטי"
date:                  2024-01-19
simple_title:         "כתיבה לפלט השגיאה הסטנדרטי"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבה לסטנדרטי השגיאה (stderr) מאפשרת להפריד הודעות שגיאה מפלט רגיל. כשתוכניות יוצרות בעיות, זה מאפשר למשתמשים ולמפתחים לראות ולטפל בהן בקלות יותר.

## איך לעשות:
```Rust
use std::io::{self, Write};

fn main() {
    // קוד רשמי
    println!("Hello, world!");
    
    // פלט לstderr
    writeln!(io::stderr(), "שגיאה: משהו השתבש!").expect("Failed to write to stderr");
}
```
תוצאה מנוהלת:

**stdout**:
```
Hello, world!
```
**stderr**:
```
שגיאה: משהו השתבש!
```

## צלילה עמוקה
הכתיבה לstderr היא שיטה שבאה מUNIX ומשמשת לפלט שגיאות לא מהווה חלק מהפלט הרגיל של התוכנית. חלופות כוללות יומנים (logs) או קבצים ייעודיים. בימפלמנטציה, `stderr` נגיש דרך `std::io` של ראסט ושימוש ב`writeln!` מאפשר כתיבה פורמטית עם מידע נוסף.

## ראה גם
- [Rust Book - std::io](https://doc.rust-lang.org/std/io/index.html)
- [Unix סטנדרטי Streams](https://en.wikipedia.org/wiki/Standard_streams)
