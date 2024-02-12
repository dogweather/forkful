---
title:                "שרבוב מחרוזת"
aliases: - /he/rust/interpolating-a-string.md
date:                  2024-01-20T17:52:01.924886-07:00
model:                 gpt-4-1106-preview
simple_title:         "שרבוב מחרוזת"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/interpolating-a-string.md"
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
