---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:58.389948-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05E1\u05E4\u05E8\
  \u05D9\u05D9\u05EA \u05D4\u05E1\u05D8\u05E0\u05D3\u05E8\u05D8 \u05E9\u05DC Rust\
  \ \u05D0\u05D9\u05E0\u05D4 \u05DB\u05D5\u05DC\u05DC\u05EA \u05E4\u05E8\u05E1\u05D5\
  \u05E8 \u05E9\u05DC \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD \u05D1\u05D0\u05D5\
  \u05E4\u05DF \u05D9\u05E9\u05D9\u05E8, \u05D0\u05DA \u05D4-`chrono` crate \u05E9\
  \u05D1\u05E9\u05D9\u05DE\u05D5\u05E9 \u05E0\u05E8\u05D7\u05D1 \u05D4\u05D5\u05D0\
  \ \u05E4\u05EA\u05E8\u05D5\u05DF \u05E2\u05DE\u05D9\u05D3 \u05DC\u05D4\u05EA\u05DE\
  \u05D5\u05D3\u05D3\u05D5\u05EA \u05E2\u05DD \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\
  \u05DD \u05D5\u05D6\u05DE\u05E0\u05D9\u05DD.\u2026"
lastmod: '2024-03-13T22:44:39.002670-06:00'
model: gpt-4-0125-preview
summary: "\u05E1\u05E4\u05E8\u05D9\u05D9\u05EA \u05D4\u05E1\u05D8\u05E0\u05D3\u05E8\
  \u05D8 \u05E9\u05DC Rust \u05D0\u05D9\u05E0\u05D4 \u05DB\u05D5\u05DC\u05DC\u05EA\
  \ \u05E4\u05E8\u05E1\u05D5\u05E8 \u05E9\u05DC \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\
  \u05DD \u05D1\u05D0\u05D5\u05E4\u05DF \u05D9\u05E9\u05D9\u05E8, \u05D0\u05DA \u05D4\
  -`chrono` crate \u05E9\u05D1\u05E9\u05D9\u05DE\u05D5\u05E9 \u05E0\u05E8\u05D7\u05D1\
  \ \u05D4\u05D5\u05D0 \u05E4\u05EA\u05E8\u05D5\u05DF \u05E2\u05DE\u05D9\u05D3 \u05DC\
  \u05D4\u05EA\u05DE\u05D5\u05D3\u05D3\u05D5\u05EA \u05E2\u05DD \u05EA\u05D0\u05E8\
  \u05D9\u05DB\u05D9\u05DD \u05D5\u05D6\u05DE\u05E0\u05D9\u05DD."
title: "\u05E4\u05E8\u05E1\u05D5\u05DD \u05EA\u05D0\u05E8\u05D9\u05DA \u05DE\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA"
weight: 30
---

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
