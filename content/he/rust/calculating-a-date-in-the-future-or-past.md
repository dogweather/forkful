---
title:                "Rust: חישוב תאריך בעתיד או בעבר למסגרת תכנות מחשבים."
simple_title:         "חישוב תאריך בעתיד או בעבר למסגרת תכנות מחשבים."
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## למה

למה לחשוב על חישוב תאריך בעתיד או בעבר? חישוב תאריך יכול להיות אמצעי מאוד שימושי בתוך תכנות, במיוחד כאשר יש צורך לנהל טווחי תאריכים ולהבין תאריכים אחרים בקוד.

## איך לעשות זאת

כדי לחשב תאריך בעתיד או בעבר בשפת ראסט, נוכל להשתמש בפונקציות מובנות וחיצוניות. נוכל להשתמש בפונקציות כגון `chrono` על מנת ליצור אובייקטי תאריך וזמן, ובפונקציות כגון `DateTime::format` על מנת להציג את התאריך בפורמט מבוקש.

```Rust
// כדי לחשב תאריך יום אחד לפני היום הנוכחי
use chrono::{DateTime, Duration, Utc};

let now: DateTime<Utc> = Utc::now();
let one_day_ago: DateTime<Utc> = now - Duration::days(1);

println!("תאריך לפני יום אחד הוא: {}", one_day_ago.format("%d/%m/%Y"));

// כדי לחשב תאריך שנה אחת לאחר היום הנוכחי
use chrono::{DateTime, Duration, Utc};

let now: DateTime<Utc> = Utc::now();
let one_year_from_now: DateTime<Utc> = now + Duration::days(365);

println!("תאריך לאחר שנה הוא: {}", one_year_from_now.format("%d/%m/%Y"));
```

## חקירה מעמיקה

חישובי תאריך יכולים להיות מורכבים כאשר ישנם טווחי תאריכים מרובים או כאשר יש צורך לנהל תאריכים בגבולות זמניים שונים. בנוסף, בשפת ראסט ישנן פונקציות נוספות כמו `TimeZone` ו- `Duration` שיכולות לעזור בחישוב וניהול תאריכים מורכבים.

## ראו גם

למידע נוסף על חישובי תאריך בשפת ראסט, ראו את הקישורים הבאים:

- דוקומנטציה רשמית של חבילת `chrono`: https://docs.rs/chrono/
- מדריך קוד בעברית על השימוש בפונקציות `chrono` בראסט: https://madewithrust.com/blog/date-time-calcul