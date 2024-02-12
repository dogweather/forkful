---
title:                "פרסום תאריך ממחרוזת"
aliases:
- he/rust/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:15:58.389948-07:00
model:                 gpt-4-0125-preview
simple_title:         "פרסום תאריך ממחרוזת"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

פענוח תאריך ממחרוזת הוא משימה נפוצה בעת התמודדות עם קלט ממשתמש או קריאת נתונים מקבצים, שכוללת המרה של נתוני מחרוזת לפורמט תאריך שמזוהה על ידי שפת התכנות. ב-Rust, זה חיוני לפעולות על תאריכים, כמו השוואות, חשבון או עיצוב, וזה משפר את תקפות הנתונים ושלמותם באפליקציות.

## איך לעשות:

### באמצעות ספריית הסטנדרט של Rust (`chrono` Crate)
ספריית הסטנדרט של Rust אינה כוללת פרסור של תאריכים באופן ישיר, אך ה-`chrono` crate שבשימוש נרחב הוא פתרון עמיד להתמודדות עם תאריכים וזמנים. ראשית, הוסף את `chrono` ל-`Cargo.toml` שלך:

```toml
[dependencies]
chrono = "0.4"
```

לאחר מכן, השתמש ב-`chrono` כדי לפרסר מחרוזת תאריך לאובייקט `NaiveDate`:

```rust
extern crate chrono;
use chrono::NaiveDate;

fn main() {
    let date_str = "2023-04-01";
    let date = NaiveDate::parse_from_str(date_str, "%Y-%m-%d")
        .expect("Failed to parse date");

    println!("Parsed date: {}", date);
}

// פלט לדוגמא:
// Parsed date: 2023-04-01
```

### באמצעות הטיפול המתקדם של Rust בתאריך-זמן (`time` Crate)
לטיפול מתקדם יותר בתאריך-זמן, כולל פרסור יותר ארגונומי, שקול להשתמש ב-`time` crate. ראשית, כלול אותו ב-`Cargo.toml` שלך:

```toml
[dependencies]
time = "0.3"
```

לאחר מכן, פרסר מחרוזת תאריך באמצעות הטיפוס `Date` ו-`PrimitiveDateTime`:

```rust
use time::{Date, PrimitiveDateTime, macros::datetime};

fn main() {
    let date_str = "2023-04-01 12:34:56";
    let parsed_date = PrimitiveDateTime::parse(
        date_str, 
        &datetime!("%Y-%m-%d %H:%M:%S")
    ).expect("Failed to parse date and time");

    println!("Parsed datetime: {}", parsed_date);
}

// פלט לדוגמא:
// Parsed datetime: 2023-04-01 12:34:56
```

שני הדוגמאות מציגות איך Rust, בעזרת חבילות של צד שלישי, מקל על פרסור מחרוזות תאריך לאובייקטי תאריך שניתן לטפל בהם, הופך אותה לכלי עוצמתי לפיתוח תוכנה הכרוך בנתונים זמניים.
