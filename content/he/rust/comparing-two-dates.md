---
title:                "השוואה בין שני תאריכים"
html_title:           "Arduino: השוואה בין שני תאריכים"
simple_title:         "השוואה בין שני תאריכים"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## מה ולמה?

השוואת שני תאריכים היא דרך לקבוע איזה תאריך הגיע לפני השני או אם שני התאריכים הם אותו התאריך. מתכנתים משתמשים בזה כדי לנתב את התנהגות התוכנה בהתאם לתאריכים שונים.

## איך לבצע:

היינו מבצעים השוואת תאריכים ב-Rust על ידי שימוש בספריית Chrono. בואו נראה חלק מהקוד:

```Rust
extern crate chrono;

use chrono::{DateTime, FixedOffset};

fn main() {
    let date1: DateTime<FixedOffset> = DateTime::parse_from_rfc3339("2022-01-01T12:00:00+01:00").unwrap();
    let date2: DateTime<FixedOffset> = DateTime::parse_from_rfc3339("2022-01-01T12:01:00+01:00").unwrap();

    if date1 < date2 {
        println!("date1 is before date2");
    } else if date1 == date2 {
        println!("date1 is equal to date2");
    } else {
        println!("date1 is after date2");
    }
}
```

אם הפעלנו את הקוד הזה, תוצאת ההדפסה שלנו הייתה: `date1 is before date2`.

## צלילה עמוקה:

השוואת תאריכים היא פעולה בסיסית בתכנות שעשויה להיראות פשוטה, אך מורכבת בהתארח המשתנים של מערכות זמן מקומיות. ב-Rust ישנם מספר מרחבים של שמות שמספקים מחלקות זמן אחרות, אבל ספריית Chrono היא הנפוצה ביותר ברוסטשטיין.

## ראה גם:

למידע נוסף, שיעורים נוספים, או פרקטיקות מומלצות, בקרו במשאבים הבאים:

* [Chrono Documentation](https://docs.rs/chrono/0.4.19/chrono/)
* [Rust Date & Time Tutorial](https://www.tutorialspoint.com/rust/rust_date_time.htm)
* [Rust Programming Language Official Documentation](https://doc.rust-lang.org/std/)