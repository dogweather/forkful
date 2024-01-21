---
title:                "גילוי מספרים אקראיים"
date:                  2024-01-20T17:50:37.927062-07:00
model:                 gpt-4-1106-preview
simple_title:         "גילוי מספרים אקראיים"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## מה ולמה?
הפקת מספרים אקראיים היא יצירת ערכים בלתי צפויים שמשחקים תפקיד חשוב למשל באבטחת מידע, משחקים וניסויים סטטיסטיים. מתכנתים משתמשים בזה כדי להבטיח שהתוצאות שלהם לא יהיו צפויות מראש.

## איך לעשות:
כדי לפקת מספרים אקראיים ב-Rust, תשתמש בקרייט rust-random. הוסף אותו ל-Cargo.toml שלך ואז השתמש בו בקוד שלך.

```Rust
use rand::{Rng, thread_rng};

fn main() {
    let mut rng = thread_rng();
    let n: i32 = rng.gen_range(0..100);
    println!("מספר אקראי: {}", n);
}
```

דוגמא לפלט:
```
מספר אקראי: 42
```

## טבילה עמוקה:
היסטוריה: הרעיון של יצירת מספרים אקראיים הוא ישן כמו המתמטיקה עצמה, אבל רק במאה ה-20 התחילו לפתח שיטות חישוב אקראיות.

אלטרנטיבות: חוץ מהקרייט rand, יש גם אפשרויות אחרות כמו getrandom או פונקציות מובנות במערכת ההפעלה.

פרטי מימוש: ב-Rust, צגי אקראיות (RNGs) עובדים על מנגנונים קריפטוגרפיים ליצירת האקראיות, זה מאוד חשוב לענייני אבטחה והבטחת יצירת מספרים באמת לא צפויים.

## ראו גם:
- התיעוד של [rand](https://docs.rs/rand/latest/rand/)
- דיון בפורום [Rust users](https://users.rust-lang.org/) אודות הפקת אקראיות
- מאמר על [אלגוריתמים של יצירת מספרים אקראיים](https://en.wikipedia.org/wiki/Random_number_generation)