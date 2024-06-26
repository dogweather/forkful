---
date: 2024-01-20 17:38:08.086788-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1-Rust, \u05D4\
  \u05DE\u05E8\u05EA \u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\u05DE\u05D7\u05E8\u05D5\
  \u05D6\u05EA \u05D4\u05D9\u05D0 \u05E4\u05E9\u05D5\u05D8\u05D4 \u05D1\u05E2\u05D6\
  \u05E8\u05EA \u05D4\u05E1\u05E4\u05E8\u05D9\u05D4 `chrono`."
lastmod: '2024-03-13T22:44:39.006153-06:00'
model: gpt-4-1106-preview
summary: "\u05D1-Rust, \u05D4\u05DE\u05E8\u05EA \u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D4\u05D9\u05D0 \u05E4\u05E9\u05D5\u05D8\
  \u05D4 \u05D1\u05E2\u05D6\u05E8\u05EA \u05D4\u05E1\u05E4\u05E8\u05D9\u05D4 `chrono`."
title: "\u05D4\u05DE\u05E8\u05EA \u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA"
weight: 28
---

## איך לעשות:
ב-Rust, המרת תאריך למחרוזת היא פשוטה בעזרת הספריה `chrono`.

```rust
extern crate chrono;
use chrono::{DateTime, Utc, Local};

fn main() {
    // קבלת תאריך ושעה עכשוויים ב-UTC
    let now_utc: DateTime<Utc> = Utc::now();
    // המרה למחרוזת בפורמט RFC 2822
    let string_utc_rfc2822 = now_utc.to_rfc2822();
    println!("UTC now in RFC 2822: {}", string_utc_rfc2822);

    // קבלת תאריך ושעה עכשוויים מקומיים
    let now_local: DateTime<Local> = Local::now();
    // המרה למחרוזת בפורמט אישי
    let string_local_custom = now_local.format("%d/%m/%Y %H:%M:%S").to_string();
    println!("Local now in custom format: {}", string_local_custom);
}
```

תוצאות דוגמה:
```
UTC now in RFC 2822: Thu, 3 Feb 2023 17:40:58 +0000
Local now in custom format: 03/02/2023 19:40:58
```

## שיקול עמוק יותר
בחירת פורמט התאריך תלויה בהקשר. הספריה `chrono` ב-Rust מספקת גמישות ענקית לצורך כך. בעבר, פורמטים כגון RFC 2822 היו נפוצים לשמירת זמן בדוא"ל ו-HTTP. פורמטים מותאמים אישית מאפשרים תמיכה באזורים זמן מקומיים והעדפות משתמש. לידע נוסף, בחן את התיעוד של `chrono` ושקול שימוש בספריות חיצוניות למשימות מיוחדות יותר.

## ראה גם
- תיעוד ספריית `chrono` ל-Rust: [https://docs.rs/chrono/](https://docs.rs/chrono/)
- גייד לפורמטים שונים של תאריכים ושעות: [https://docs.rs/chrono/*/chrono/format/strftime/index.html](https://docs.rs/chrono/*/chrono/format/strftime/index.html)
- דוגמאות נוספות ומידע על טיפול בתאריכים ושעות ב-Rust: [https://doc.rust-lang.org/book/ch10-00-generics.html](https://doc.rust-lang.org/book/ch10-00-generics.html)
