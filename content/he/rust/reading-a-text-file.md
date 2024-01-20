---
title:                "קריאת קובץ טקסט"
html_title:           "Go: קריאת קובץ טקסט"
simple_title:         "קריאת קובץ טקסט"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## מה ולמה?

קריאת קובץ טקסט זהו פעולה בה המחשב קורא מידע מהדיסק לזכרון. מתכנתים קוראים קבצי טקסט לביצוע תהליכים למידע שהם מכילים.

## איך לעשות:

```Rust
use std::fs::File;
use std::io::prelude::*;

fn main() -> std::io::Result<()> {
    let mut file = File::open("test.txt")?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    println!("Content: {}", contents);
    Ok(())
}
```

ברצות הקוד מעלה, תוכל לקרוא את התוכן של קובץ "test.txt" ולהדפיס אותו.

## צלילה עמוקה:

במקור, הקריאה של קבצים הייתה דרך מרכזית לתקשורת בין מחשבים בעזרת העברת קבצים. יש דרכים אחרות לקרוא קבצים, אך המערך המסוים של Rust לקריאת קבצים הוא אחד הפשוטים ביותר לשימוש. בעזרת שיטות אלו, Rust מבצע אוטומטית את חילוף הנתונים בין המערכת והזכרון.

## ראה גם:

1. [תיעוד ה-Rust](https://doc.rust-lang.org/book/ch04-02-references-and-borrowing.html)
2. [מדריך לקריאת קבצים ב-Rust על StackOverflow](https://stackoverflow.com/questions/31192956/whats-the-de-facto-way-of-reading-and-writing-files-in-rust-1-x)