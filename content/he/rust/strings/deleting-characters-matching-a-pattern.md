---
title:                "מחיקת תווים התואמים לתבנית"
aliases:
- /he/rust/deleting-characters-matching-a-pattern/
date:                  2024-01-20T17:43:48.293500-07:00
model:                 gpt-4-1106-preview
simple_title:         "מחיקת תווים התואמים לתבנית"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## מה ולמה?
מחיקת תווים לפי תבנית היא פעולה שבה מסירים ממחרוזת כל התווים התואמים לתבנית מסוימת. תכנתים עושים את זה לניקוי נתונים, לקבלת פורמט רצוי או לאימות קלט.

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
