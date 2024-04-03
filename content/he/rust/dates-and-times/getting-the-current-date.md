---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:23.175683-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: #."
lastmod: '2024-03-13T22:44:39.004481-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "\u05E7\u05D1\u05DC\u05EA \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05D4\u05E0\
  \u05D5\u05DB\u05D7\u05D9"
weight: 29
---

## איך לעשות:


### באמצעות ספריית הסטנדרט של Rust
ספריית הסטנדרט של Rust מספקת דרך מוגבלת אך מהירה לקבל את הזמן הנוכחי, אך לא באופן ישיר את התאריך הנוכחי בפורמט לוח שנה. הנה איך עושים זאת:

```rust
use std::time::{SystemTime, UNIX_EPOCH};

fn main() {
    match SystemTime::now().duration_since(UNIX_EPOCH) {
        Ok(n) => println!("זמן נוכחי: {} שניות מאז העידן היוניקסי.", n.as_secs()),
        Err(_) => panic!("SystemTime לפני העידן היוניקסי!"),
    }
}
```

פלט:
```
זמן נוכחי: 1615390665 שניות מאז העידן היוניקסי.
```

### באמצעות ספריית ה-Chrono
לפונקציונליות מתקדמת יותר של תאריך ושעה, כולל אחזור התאריך הנוכחי, עליך להשתמש בספריית `chrono`. ראשית, הוסף את `chrono` ל-`Cargo.toml` שלך:

```toml
[dependencies]
chrono = "0.4"
```

לאחר מכן, אתה יכול להשתמש ב-`chrono` כדי לקבל את התאריך הנוכחי:

```rust
extern crate chrono;
use chrono::{Local, Datelike};

fn main() {
    let now = Local::now();
    println!("תאריך נוכחי: {}-{}-{}", now.year(), now.month(), now.day());
}
```

פלט:
```
תאריך נוכחי: 2023-4-20
```

ספריית ה-`chrono` מקלה על העבודה עם תאריכים ושעות, ומציעה מגוון רחב של פונקציונליות מעבר לאחזור התאריך הנוכחי, כולל ניתוח, עיצוב, ופעולות אריתמטיות על תאריכים ושעות.
