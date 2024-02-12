---
title:                "קבלת התאריך הנוכחי"
date:                  2024-02-03T19:11:23.175683-07:00
model:                 gpt-4-0125-preview
simple_title:         "קבלת התאריך הנוכחי"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

אחזור התאריך הנוכחי ב-Rust הוא משימה נפוצה לצורכים כגון רישום לוגים, פעולות מבוססות זמן, או פשוט להצגת התאריך. בניגוד לחלק מהשפות שכוללות פונקציונליות של תאריך ושעה בספרייה הסטנדרטית שלהן, Rust מעודדת את השימוש בספרייה צד שלישי עמידה, chrono, לניפוי תאריך ושעה מקיף בשל הפונקציונליות העליונה ונוחות השימוש שלה.

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
