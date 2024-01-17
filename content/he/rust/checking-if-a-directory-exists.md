---
title:                "בדיקה האם תיקייה קיימת"
html_title:           "Rust: בדיקה האם תיקייה קיימת"
simple_title:         "בדיקה האם תיקייה קיימת"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## מה ולמה?

כשתפתחו תוכנית בראסט, ייתכן שתצטרך לבדוק אם קיימת תיקייה במערכת הקבצים. זהו תהליך חשוב בתכנות המאפשר לנו להתאים את הקוד למצבים שונים ולהתחשב באופני התנהגות שונים שייתכנו בטרמינל או בפני המשתמש.

## איך לעשות זאת:

```Rust
use std::fs;
fs::metadata("<directory_path>")?.is_dir();
```

הקוד הזה יבדוק אם התיקייה שהתעתקה למשתנה `directory_path` קיימת במערכת הקבצים. אם כן, יוחזר ערך `true`, אחרת יוחזר ערך `false`.

## מערכת מתקדמת:

בעקבות טכנולוגיית ה-POSIX שפותחה בשנות ה-80 של המאה הקודמת, רבות ממערכות ההפעלה המודרניות מאפשרות לנו לבדוק אם קיימת תיקייה במערכת הקבצים. ב-Rust, ניתן לבדוק זאת באמצעות פונקציות כמו `fs::metadata()` או `fs::open()` שמקבלות את הנתיב של התיקייה כפרמטר.

## ראה גם:

- [מדריך לפיתוח מתקדם ב- Rust](https://www.geeksforgeeks.org/rust-programming-language/advanced-rust-programming-tutorials/)
- [מדריך לניהול תיקיות וקבצים באמצעות fs](https://doc.rust-lang.org/std/fs/index.html)
- [המאמר המקורי בעברית](https://www.geeksforgeeks.org/rust-programming-language/)