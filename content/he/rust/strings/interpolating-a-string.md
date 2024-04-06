---
date: 2024-01-20 17:52:01.924886-07:00
description: "How to: ` \u05E4\u05DC\u05D8 \u05D3\u05D5\u05D2\u05DE\u05D0."
lastmod: '2024-04-05T21:53:40.222450-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u05E9\u05E8\u05D1\u05D5\u05D1 \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA"
weight: 8
---

## How to:
````Rust
fn main() {
    let name = "דני";
    println!("שלום, {}!", name); // אינטרפולציה בזמן קומפילציה
}
````
פלט דוגמא:
```
שלום, דני!
```

## Deep Dive
אינטרפולציה של מחרוזות היא תכונה של שפות תכנות רבות, הגיעה ל-Rust מעולמם של שפות כמו Perl או Ruby. ב-Rust, זה נעשה בעזרת המקרו `format!` או וריאציות שלו כמו `print!` או `println!`. בניגוד לשפות רבות אחרות, Rust מבצע זאת בזמן קומפילציה, מה שאומר ששגיאות ריצה בשילוב מחרוזות הם דבר נדיר יחסית. חלופות כוללות עבודה עם פונקציות כמו `format!` ליצירת מחרוזת חדשה או שילוב של מחרוזות באמצעות פעולת החיבור.

## See Also
- [The Rust Programming Language - Formatted print](https://doc.rust-lang.org/stable/rust-by-example/hello/print.html)
- [Rust By Example - Strings](https://doc.rust-lang.org/rust-by-example/std/str.html)
- [Rust Book - More about Macros](https://doc.rust-lang.org/book/ch19-06-macros.html)
