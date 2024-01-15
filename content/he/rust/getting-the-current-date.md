---
title:                "לקבלת התאריך הנוכחי"
html_title:           "Rust: לקבלת התאריך הנוכחי"
simple_title:         "לקבלת התאריך הנוכחי"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## למה

התאריך הנוכחי הוא מידע חשוב ביותר ביישומי תוכנה רבים. מעקב אחרי התאריך הנוכחי יכול לסייע לנו ליצור תנאים מיוחדים ביישומים שלנו או להציג מידע חשוב למשתמשים. 

## איך לעשות זאת

```Rust
use std::time::{SystemTime, UNIX_EPOCH};
use chrono::{DateTime, Local};

let now = SystemTime::now();
let since_epoch = now.duration_since(UNIX_EPOCH).expect("Time went backwards");
let current_date_time = DateTime::<Local>::from(since_epoch);

println!("The current date and time is: {}", current_date_time);
```
כדי לקבל את התאריך הנוכחי בפורמט מובנה, נצטרך להשתמש בספריות `std :: time` ו- `chrono`. נתחיל על ידי ייבוא שני הספריות ואתחול המשתנים `now` ו `since_epoch` שבהם נאחז את ערך התאריך הנוכחי ואת מספר השניות הפורסת מהזמן האפיקאי מאז תחילת הפעולה. לאחר מכן, נעבוד עם `chrono ` כדי להמיר את הערך החשבוני לתאריך ושעה קונקרטיים בפורמט המקובל זמן מקומי. סוף סוף, נדפיס את המידע שלנו באמצעות הממשק המוכנס ונקבל תוצאה כזו:
```
The current date and time is: 2021-11-03 12:30:00
```

## Deep Dive

כמו שאתה רואה, תכנות בראסט לקבלת התאריך הנוכחי הוא מאוד קל ויכול להתאים לכל מטרה שלך. אם ברצונך לדעת עוד על צורת התאריך שלנו או על אופן שבו ניתן לעבוד עם משתנים זמן מיוחדים, אנו ממליצים לבדוק את התיעוד הרשמי של שפת התכנות ראסט.

## ראה גם
- [התיעוד של ראסט עבור std::time](https://doc.rust-lang.org/std/time/)
- [התיעוד של ראסט עבור chrono](https://docs.rs/chrono/0.4.19/chrono/)