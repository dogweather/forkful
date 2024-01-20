---
title:                "קריאה של ארגומנטים משורת הפקודה"
html_title:           "C#: קריאה של ארגומנטים משורת הפקודה"
simple_title:         "קריאה של ארגומנטים משורת הפקודה"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## מה זה קריאת ארגומנטים מממשק שורה פקודות ולמה אנחנו משתמשים בזה?

קריאת ארגומנטים מממשק שורת הפקודות היא התהליך של קבלת נתונים משורת הפקודות בעת הפעלת התוכנית. תכנתי משתמשים בזה כי זה מאפשר להם להעצב את הפעולה של התוכנית מבלי לפתוח קוד מקור ולשנותה ידנית.

## איך לעשות:

```Rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    println!("{:?}", args);
}
```
כאשר נפעיל את התוכנה עם כמה ארגומנטים, לדוגמה:
```bash
$ cargo run hello world
```
הפלט שנדפיס יהיה:
```bash
["target/debug/my_program", "hello", "world"]
```

## צלילה עמוקה:

האפשרות לקרוא ארגומנטים משורת הפקודה מוכרת לנו בזכות שפות תכנות כמו C ושיל. חלופות כוללות קריאת קובץ הגדרות או קליטת קלט מהמשתמש במהלך ריצת התוכנית. אפשר להניח שהן פחות נוחות מאחר שהן אינן מאפשרות לנו לשנות את התנהגות היישום באופן דינמי ואינן נותנות את האפשרות לשנות את התנהגות היישום מבלי לחדש את ההרצה.

בעזרת Rust, אנחנו יכולים לחלץ את הארגומנטים אמצעי `std::env::args()`. אנחנו משתמשים בפונקציה `collect()` כדי להמיר את האיטרטור לסוג `Vec<String>`.

## ראה גם:

- [חבילת structopt](https://docs.rs/structopt/0.3.21/structopt/) ב-crates.io, שמקלה על כתיבת מנגנון קומפלקס יותר לעיבוד ארגומנטים.
- [דוקומנטציה של std::env::args](https://doc.rust-lang.org/std/env/fn.args.html)