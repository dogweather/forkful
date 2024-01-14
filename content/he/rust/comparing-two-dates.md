---
title:                "Rust: השוואת שתי תאריכים"
simple_title:         "השוואת שתי תאריכים"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## למה?

כשמתחילים לכתוב קוד ב-Rust, עשויים להתעניין בהשוואת תאריכים. הבנת כיצד לעבוד עם תאריכים בקוד יכולה להקל על התכנות היום יום ולאפשר לנו לכתוב קוד יעיל יותר. למה לא נראה כיצד ניתן לבצע השוואת תאריכים בקוד Rust.

## כיצד לבצע השוואת תאריכים ב-Rust

ניתן לבצע השוואת תאריכים בקוד Rust באמצעות הספרייה `chrono`. נחלק את התהליך לשני שלבים: יצירת תאריך עם הפונקציה `chrono::DateTime::parse_from_str()` והשוואת שני תאריכים עם הפונקציה `DateTime::cmp()`.

```
use chrono::DateTime;
use chrono::offset::{ Local, TimeZone };

let date_string = "2020-10-15 12:00:00";
let date = DateTime::parse_from_str(date_string, "%Y-%m-%d %H:%M:%S").unwrap();

let now = Local::now();
match date.cmp(&now) {
    std::cmp::Ordering::Less => println!("התאריך עבר"),
    std::cmp::Ordering::Equal => println!("היום הוא התאריך!"),
    std::cmp::Ordering::Greater => println!("התאריך הצפוי מתקרב"),
}
```

פלט:

```
התאריך הצפוי מתקרב
```

## צפייה עמוקה

כדי לעבוד עם תאריכים מורחבים יותר כמו שנים בלבד או דיווחי שעון, ניתן להתאים את הפונקציות `chrono::DateTime::parse_from_str()` ו- `DateTime::cmp()` לצרכים שלנו. ניתן גם לבדוק את תקינות התאריכים עם הפונקציה `DateTime::naive_local()`.

## ראה גם

- [המדריך הרשמי של chrono](https://docs.rs/chrono/0.4.19/chrono/index.html)
- [וידאו לימודי על לומדת השוואת תאריכים ב-Rust עם `chrono`](https://www.youtube.com/watch?v=JZBwy48-IhE)
- [דוגמאות לעבודה עם תאריכים בקוד Rust](https://github.com/ramsayleung/rust-date-time-examples)