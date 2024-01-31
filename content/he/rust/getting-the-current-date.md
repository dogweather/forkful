---
title:                "קבלת התאריך הנוכחי"
date:                  2024-01-20T15:16:25.874012-07:00
html_title:           "C: קבלת התאריך הנוכחי"
simple_title:         "קבלת התאריך הנוכחי"

category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## מה ולמה?
לקבל את התאריך הנוכחי בקוד זה פשוט לבדוק את "היום" בתוכנה. תכניתנים צריכים את זה לתיעוד, תזמנים, ולפעמים לתכנות תנאים בהתבסס על התאריך.

## איך עושים את זה:
```Rust
use chrono::{DateTime, Local};

fn main() {
    let now: DateTime<Local> = Local::now();
    println!("Current Date and Time: {}", now.format("%d/%m/%Y %T"));
}
```
פלט לדוגמא:
```
Current Date and Time: 20/02/2023 14:12:45
```

## צלילה לעומק
בראשית ימיו, שפת רוסט לא כללה תמיכה רשמית בפונקציות זמן ותאריך. עם הזמן, הקהילה פיתחה ספריות כמו `chrono` שהפכו לסטנדרט למעשה. ישנן אלטרנטיבות כמו `time` או אפילו פונקציות מובנות בשפה באמצעות `std::time`, אבל `chrono` מספקת את הכלים העשירים והמלאים ביותר לעבודה עם תאריכים וזמנים.

## ראה גם
- [תיעוד רוסט על std::time](https://doc.rust-lang.org/std/time)
- [תיעוד רוסט רשמי](https://doc.rust-lang.org/book/)
