---
title:                "Rust: קבלת התאריך הנוכחי"
simple_title:         "קבלת התאריך הנוכחי"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

##למה
מקבלת את התאריך הנוכחי בקוד שלך יכול להיות חשוב לכמה סיבות, כגון יצירת יומן או רישום תאריכים בתוך אפליקציה.

##כיצד לעשות זאת
כדי לקבל את התאריך הנוכחי בשפת ראסט, תצטרכו להשתמש במודול המובנה שנקרא "ליבת התאריך והשעה". האתר הבא מכיל דוגמאות קוד ופלט מוכן, בתוספת תיעוד מפורט על כל פקודה:

```Rust
use std::time::{SystemTime, UNIX_EPOCH};

let current_time = SystemTime::now()
    .duration_since(UNIX_EPOCH)
    .expect("Failed to get current time");

println!("Current time in seconds since UNIX epoch: {}", current_time.as_secs());
```

פלט: "Current time in seconds since UNIX epoch: 1592987152"

##חפירה מעמיקה
ישנם כמה דרכים שונות לקבל את התאריך הנוכחי בשפת ראסט, לכן יכול להיות שימושי לבדוק שיטות נוספות כדי להבין איך הם פועלים ומתי הן התאימות לצרכים שלכם. נוסף על כך, אתם יכולים לחקור שימושים נפוצים של תאריכים ושעות בשפת ראסט כדי להתחיל להשתמש בהם בקוד שלכם.

##ראה גם
- [תיעוד רשמי על התאריך והשעה בראסט](https://doc.rust-lang.org/std/time/index.html)
- [ערוץ ראסט בפורום המפתחים הישראלי](https://he.codeforum.org/c/rust)