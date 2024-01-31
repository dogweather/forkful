---
title:                "ניתוח תאריך ממחרוזת"
date:                  2024-01-20T15:38:30.052636-07:00
html_title:           "Arduino: ניתוח תאריך ממחרוזת"
simple_title:         "ניתוח תאריך ממחרוזת"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?

פיענוח תאריך ממחרוזת הוא התהליך שבו קוד לוקח טקסט שמייצג תאריך והופך אותו למבנה נתונים שמתאר תאריך כדי שנוכל לעבוד איתו בתוכנה. מתכנתים עושים זאת כי נתוני תאריך בפורמט מחרוזת מקובלים מאוד בממשקי משתמש ובשיתוף נתונים.

## איך לעשות:

בראסט, ניתן להשתמש בחבילה `chrono` כדי לפרש תאריכים. קודם כל, הוסף את `chrono` לתלותות שלך ב-Cargo.toml:

```rust
[dependencies]
chrono = "0.4"
```

לאחר מכן, פענח תאריך ממחרוזת כך:

```rust
extern crate chrono;
use chrono::prelude::*;

fn main() {
    let text = "2023-04-05 23:10:04";
    let date = NaiveDateTime::parse_from_str(text, "%Y-%m-%d %H:%M:%S");
    
    match date {
        Ok(date) => println!("Parsed date and time: {}", date),
        Err(e) => println!("Error parsing date: {}", e),
    }
}
```

הפלט יהיה:

```
Parsed date and time: 2023-04-05 23:10:04
```

## עיון עמוק:

בעבר, פיענוח תאריכים היה תהליך כבד משום שדרש התמודדות עם מגוון פורמטים ואזורי זמן משתנים. `chrono` מודרנית פשטה את התהליך על ידי ספק פונקציות מוכנות שטופלות רוב המקרים. החלופה ל`chrono` בראסט היא להשתמש במודול `std::time`, אבל הוא מספק פחות יכולות מבחינת ניתוח תאריכים. ביצוע עם `chrono` כרוך בהמרה של המחרוזת למבנה נתונים ספציפי לתאריך (`NaiveDateTime` למשל), תוך שימוש בתבניות זמן לזיהוי הרכיבים במחרוזת. 

## ראה גם:

- [דוקומנטציה של `chrono`](https://docs.rs/chrono/0.4.19/chrono/): המקור הרשמי למידע על החבילה `chrono`.
- [Book of Rust](https://doc.rust-lang.org/book/): המדריך הרשמי לשפת ראסט, כולל פרקים על טיפול בנתוני זמן ותאריך.
- [רפרנס לstd::time](https://doc.rust-lang.org/std/time/): מידע מעמיק על טיפול בזמן ותאריך באמצעות הסטנדרט המובנה של ראסט.
