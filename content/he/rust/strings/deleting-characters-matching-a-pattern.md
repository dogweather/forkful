---
date: 2024-01-20 17:43:48.293500-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05DB\u05D3\u05D9\
  \ \u05DC\u05DE\u05D7\u05D5\u05E7 \u05EA\u05D5\u05D5\u05D9\u05DD \u05DE\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA \u05DC\u05E4\u05D9 \u05EA\u05D1\u05E0\u05D9\u05EA \u05D1\
  -Rust, \u05D0\u05E4\u05E9\u05E8 \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\u05E4\
  \u05D5\u05E0\u05E7\u05E6\u05D9\u05D4 `replace` \u05DE\u05D4\u05DE\u05D5\u05D3\u05D5\
  \u05DC `str`, \u05D0\u05D5 \u05D1\u05D7\u05D1\u05D9\u05DC\u05EA \u05E8\u05D2\u05D5\
  \u05DC\u05E8 \u05D0\u05E7\u05E1\u05E4\u05E8\u05E9\u05E0\u05E1 \u05D7\u05D9\u05E6\
  \u05D5\u05E0\u05D9\u05EA \u05DB\u05DE\u05D5 `regex`. \u05DC\u05D4\u05DC\u05DF\u2026"
lastmod: '2024-03-13T22:44:38.961173-06:00'
model: gpt-4-1106-preview
summary: "\u05DB\u05D3\u05D9 \u05DC\u05DE\u05D7\u05D5\u05E7 \u05EA\u05D5\u05D5\u05D9\
  \u05DD \u05DE\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\u05E4\u05D9 \u05EA\u05D1\
  \u05E0\u05D9\u05EA \u05D1-Rust, \u05D0\u05E4\u05E9\u05E8 \u05DC\u05D4\u05E9\u05EA\
  \u05DE\u05E9 \u05D1\u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D4 `replace` \u05DE\u05D4\
  \u05DE\u05D5\u05D3\u05D5\u05DC `str`, \u05D0\u05D5 \u05D1\u05D7\u05D1\u05D9\u05DC\
  \u05EA \u05E8\u05D2\u05D5\u05DC\u05E8 \u05D0\u05E7\u05E1\u05E4\u05E8\u05E9\u05E0\
  \u05E1 \u05D7\u05D9\u05E6\u05D5\u05E0\u05D9\u05EA \u05DB\u05DE\u05D5 `regex`."
title: "\u05DE\u05D7\u05D9\u05E7\u05EA \u05EA\u05D5\u05D5\u05D9\u05DD \u05D4\u05EA\
  \u05D5\u05D0\u05DE\u05D9\u05DD \u05DC\u05EA\u05D1\u05E0\u05D9\u05EA"
weight: 5
---

## איך לעשות:
כדי למחוק תווים ממחרוזת לפי תבנית ב-Rust, אפשר להשתמש בפונקציה `replace` מהמודול `str`, או בחבילת רגולר אקספרשנס חיצונית כמו `regex`. להלן דוגמאות:

בסיסי ללא regex:
```Rust
fn main() {
    let phrase = "Hello, 123 World! 456";
    let only_letters = phrase.chars().filter(|c| c.is_alphabetic()).collect::<String>();
    println!("{}", only_letters); // ידפיס "HelloWorld"
}
```

עם regex:
```Rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"\d").unwrap(); // תבנית למציאת ספרות
    let phrase = "Hello, 123 World! 456";
    let no_digits = re.replace_all(&phrase, "");
    println!("{}", no_digits); // ידפיס "Hello,  World! "
}
```

## הצצה לעומק:
בעבר, שפות רבות השתמשו בפונקציות כמו `replace` ו`remove` כדי לעבד מחרוזות. Rust מציעה ממשק אינטואיטיבי למחיקת תווים עם המתודות שלו, כמו `chars` ו`filter`. חבילות פנימיות מספקות פונקציונליות בסיסית, אבל עבור תבניות מורכבות יותר נפוצה השימוש בחבילת `regex` שמאוד עוצמתית.

המימוש הפנימי של מחיקות ב-Rust מתבצע באמצעות איטרציה על התווים ובדיקה אם הם מתאימים לתנאים (כמו בדוגמה הבסיסית), או על ידי יצירת מחרוזת חדשה בלי התווים הנבחרים (כמו בשימוש ב-regex). חלק מהיתרונות של `regex` כוללים גמישות רבה יותר בחיפוש תבניות ותמיכה בסטנדרטים של רגולר אקספרשנס.

## ראה גם:
- [רשמית Rust `std::str`](https://doc.rust-lang.org/std/str/)
- [מדריך לרגולר אקספרשנס](https://docs.rs/regex/1.3.9/regex/#syntax)
