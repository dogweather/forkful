---
date: 2024-01-20 17:46:49.752468-07:00
description: "How to: \u05D4\u05D9\u05DB\u05D5\u05DC\u05EA \u05DC\u05D7\u05DC\u05E5\
  \ \u05EA\u05EA-\u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05E9\u05D9\u05DE\u05D5\
  \u05E9\u05D9\u05EA \u05D1\u05DE\u05D9\u05D5\u05D7\u05D3 \u05D1\u05EA\u05D9\u05E2\
  \u05D5\u05D3 \u05D5\u05E0\u05D9\u05EA\u05D5\u05D7 \u05E9\u05DC \u05D8\u05E7\u05E1\
  \u05D8\u05D9\u05DD. \u05D1-Rust, \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D0\u05D9\
  \u05E0\u05E0\u05D4 \u05E4\u05E9\u05D5\u05D8 \u05E8\u05E9\u05D9\u05DE\u05D4 \u05E9\
  \u05DC \u05EA\u05D5\u05D5\u05D9\u05DD \u05D0\u05DC\u05D0 \u05E8\u05E6\u05E3 \u05E9\
  \u05DC \u05D9\u05D7\u05D9\u05D3\u05D5\u05EA UTF-8. \u05DC\u05DB\u05DF, \u05D7\u05D9\
  \u05DC\u05D5\u05E5 \u05EA\u05EA-\u2026"
lastmod: '2024-04-05T21:53:40.224921-06:00'
model: gpt-4-1106-preview
summary: "\u05D4\u05D9\u05DB\u05D5\u05DC\u05EA \u05DC\u05D7\u05DC\u05E5 \u05EA\u05EA\
  -\u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05E9\u05D9\u05DE\u05D5\u05E9\u05D9\
  \u05EA \u05D1\u05DE\u05D9\u05D5\u05D7\u05D3 \u05D1\u05EA\u05D9\u05E2\u05D5\u05D3\
  \ \u05D5\u05E0\u05D9\u05EA\u05D5\u05D7 \u05E9\u05DC \u05D8\u05E7\u05E1\u05D8\u05D9\
  \u05DD."
title: "\u05D7\u05D9\u05DC\u05D5\u05E5 \u05EA\u05EA-\u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05D5\u05EA"
weight: 6
---

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
