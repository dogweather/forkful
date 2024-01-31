---
title:                "חילוץ תת-מחרוזות"
date:                  2024-01-20T17:46:49.752468-07:00
model:                 gpt-4-1106-preview
simple_title:         "חילוץ תת-מחרוזות"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?
מה זה חילוץ תת-מחרוזות ולמה זה נעשה? חילוץ תת-מחרוזות זה לקחת חלק ממחרוזת קיימת. זה נעשה כשרוצים לעבוד או לצג רק חלק מהמידע, ולא כל המחרוזת.

## How to:
```Rust
fn main() {
    let s = "שלום עולם".to_string();
    
    let hello = &s[0..10]; // בחירת תת-מחרוזת בטווח האינדקסים
    println!("{}", hello); // ידפיס "שלום"
    
    let world = &s[11..]; // מקבל כל התווים מהאינדקס ה-11 ועד הסוף
    println!("{}", world); // ידפיס "עולם"
}
```

פלט:
```
שלום
עולם
```

## Deep Dive
היכולת לחלץ תת-מחרוזות שימושית במיוחד בתיעוד וניתוח של טקסטים. ב-Rust, מחרוזת איננה פשוט רשימה של תווים אלא רצף של יחידות UTF-8. לכן, חילוץ תת-מחרוזת דורש זהירות עם האינדקסים שלא לחתוך תווים באמצע. אלטרנטיבות כוללות שיטות כמו `split` או `find` לטיפול במחרוזות לפי תבניות. החל מ-גרסה 1.0, Rust העניקה תמיכה מובנית בניתוח מחרוזות תוך שימת דגש על בטיחות זיכרון ותקינות עם UTF-8.

## See Also
- [Rust Documentation on String](https://doc.rust-lang.org/std/string/struct.String.html)
- [Rust Programming Textbook - Strings](https://doc.rust-lang.org/book/ch08-02-strings.html)
- [The Rust Standard Library API - Slice](https://doc.rust-lang.org/std/slice/)
