---
title:                "המרת תאריך למחרוזת"
html_title:           "Bash: המרת תאריך למחרוזת"
simple_title:         "המרת תאריך למחרוזת"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## מה זה ולמה?
המרת תאריך למחרוזת היא תהליך שבו אנו משנים את המבנה של התאריך לטקסט.
אנו מבצעים זאת כדי שנוכל להציג את הנתונים באופן נוח ומובנה יותר למשתמש, או לשמירה על יעילות בעת שמירה ואחזור של הנתונים.


## איך לעשות:
במקרה של שפת Rust, אנו יכולים להמיר תאריך למחרוזת בקלות באמצעות הפונקציה `format!`.

עיקר הקוד :
```Rust
use chrono::prelude::*;

fn date_to_string(date: DateTime<Utc>) -> String {
    format!("{}", date.format("%Y-%m-%d %H:%M:%S"))
}

fn main() {
    let date = Utc::now();
    let date_string = date_to_string(date);
    println!("{}", date_string);
}
```

הפלט של הקוד :
```Rust
2022-01-01 00:00:00
```

## הצצה מעמיקה:
1. בעבר, המרת תאריך למחרוזת הייתה מסובכת בהרבה בזכות הניהיליזם והחבילות השונות של שליטת זמן. שפת Rust מקלה עלינו באמצעות חבילת `chrono`.
2. כמובן, ישנן אופציות אלטרנטיביות, כגון שימוש בפונקציה `date.format.to_string()`, אך היא פחות מנועלת ונותנת פחות שליטה בפורמט של המחרוזת.
3. הפונקציה `format!` מעריכה את המחרוזת שניתן לה, כאשר היא משנה את ה-`%Y, %m, %d, %H, %M, %S` התואמים את השנה, החודש, היום, השעה, הדקה והשנייה של התאריך שאנו מעבירים לה.

## ראה גם:

[תיעוד Rust על הפונקציה format!](https://doc.rust-lang.org/std/fmt/)

[תיעוד חבילת chrono של Rust](https://docs.rs/chrono/0.4.19/chrono/) 

[שיחת פורום בנושא תאריכים ומחרוזות ב-Rust](https://users.rust-lang.org/t/dates-and-strings/1966)