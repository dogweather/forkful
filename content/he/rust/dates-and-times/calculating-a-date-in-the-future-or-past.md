---
date: 2024-01-20 17:32:25.527751-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05E4\u05DC\u05D8\
  \ \u05D3\u05D5\u05D2\u05DE\u05D0."
lastmod: '2024-04-05T21:53:40.257704-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u05D7\u05D9\u05E9\u05D5\u05D1 \u05EA\u05D0\u05E8\u05D9\u05DA \u05D1\u05E2\
  \u05EA\u05D9\u05D3 \u05D0\u05D5 \u05D1\u05E2\u05D1\u05E8"
weight: 26
---

## איך לעשות:
```Rust
use chrono::{DateTime, Duration, Utc};

fn main() {
    let now = Utc::now(); // זמן נוכחי
    println!("Now: {}", now);

    let future_date = now + Duration::days(30); // חישוב תאריך 30 ימים בעתיד
    println!("Future date: {}", future_date);

    let past_date = now - Duration::weeks(3); // חישוב תאריך 3 שבועות בעבר
    println!("Past date: {}", past_date);
}
```

פלט דוגמא:
```
Now: 2023-04-05T14:30:05.123456789Z
Future date: 2023-05-05T14:30:05.123456789Z
Past date: 2023-03-15T14:30:05.123456789Z
```

## עיון מעמיק
המנגנון לחישוב תאריכים בעבר ובעתיד הוא חלק בלתי נפרד ממערכות מידע רבות. בעבר, הוצאנו לפועל חישובים אלה ידנית בעזרת שיטות מתמטיות מסורתיות או בעזרת יומנים ולוחות שנה. היום, כלי כמו הספרייה `chrono` בראסט מאפשרים חישובים מהירים ומדויקים ללא מאמץ רב.

ישנם אלטרנטיבות ל-`chrono`, כמו הספרייה הסטנדרטית המציעה פונקציונליות מוגבלת יותר לניהול זמן ותאריכים. הבחירה ב`chrono` נעשית בגלל האפשרויות הרחבות והממשק הנוח שלה. קחו בחשבון גם אילוצים של אזורי זמן ושמירה על תאימות לשינויים בלוח השנה, כמו קפיצות שנה.

## ראו גם
- [תיעוד Rust לספריית chrono](https://docs.rs/chrono/)
- [תיעוד Rust על טיפול בתאריכים וזמנים](https://doc.rust-lang.org/stable/std/time/)
- [המדריך המלא למודול std::time](https://doc.rust-lang.org/book/ch10-02-traits.html)
- [הבלוג הרשמי של Rust](https://blog.rust-lang.org/) עבור עדכונים וטיפים נוספים
