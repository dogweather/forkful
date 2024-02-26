---
date: 2024-01-20 17:52:01.924886-07:00
description: "\u05DE\u05D4 \u05D6\u05D4 \u05D0\u05D9\u05E0\u05D8\u05E8\u05E4\u05D5\
  \u05DC\u05E6\u05D9\u05D4 \u05E9\u05DC \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA\
  \ \u05D5\u05DC\u05DE\u05D4 \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\
  \u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4? \u05D0\u05D9\u05E0\u05D8\u05E8\u05E4\
  \u05D5\u05DC\u05E6\u05D9\u05D4 \u05E9\u05DC \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\
  \u05EA \u05DE\u05D0\u05E4\u05E9\u05E8\u05EA \u05DC\u05D4\u05DB\u05E0\u05D9\u05E1\
  \ \u05E2\u05E8\u05DB\u05D9\u05DD \u05DE\u05E9\u05EA\u05E0\u05D9\u05DD \u05DC\u05EA\
  \u05D5\u05DA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D1\u05E7\u05DC\u05D5\u05EA\
  . \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\
  \u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05D9\u05E6\u05D5\u05E8\u2026"
lastmod: '2024-02-25T18:49:37.203720-07:00'
model: gpt-4-1106-preview
summary: "\u05DE\u05D4 \u05D6\u05D4 \u05D0\u05D9\u05E0\u05D8\u05E8\u05E4\u05D5\u05DC\
  \u05E6\u05D9\u05D4 \u05E9\u05DC \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05D5\
  \u05DC\u05DE\u05D4 \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\
  \u05D9\u05DD \u05D0\u05EA \u05D6\u05D4? \u05D0\u05D9\u05E0\u05D8\u05E8\u05E4\u05D5\
  \u05DC\u05E6\u05D9\u05D4 \u05E9\u05DC \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA\
  \ \u05DE\u05D0\u05E4\u05E9\u05E8\u05EA \u05DC\u05D4\u05DB\u05E0\u05D9\u05E1 \u05E2\
  \u05E8\u05DB\u05D9\u05DD \u05DE\u05E9\u05EA\u05E0\u05D9\u05DD \u05DC\u05EA\u05D5\
  \u05DA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D1\u05E7\u05DC\u05D5\u05EA. \u05DE\
  \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA\
  \ \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05D9\u05E6\u05D5\u05E8\u2026"
title: "\u05E9\u05E8\u05D1\u05D5\u05D1 \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA"
---

{{< edit_this_page >}}

## What & Why?
מה זה אינטרפולציה של מחרוזות ולמה מתכנתים עושים את זה? אינטרפולציה של מחרוזות מאפשרת להכניס ערכים משתנים לתוך מחרוזת בקלות. מתכנתים עושים את זה כדי ליצור טקסט דינאמי, לעיתים תוך שילוב נתונים שמגיעים במהלך זמן הריצה.

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
